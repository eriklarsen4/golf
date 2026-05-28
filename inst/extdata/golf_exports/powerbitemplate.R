library(tidyverse)
library(apexcharter)
library(fs)
library(gt)
library(lubridate)
library(purrr)
library(htmlwidgets)


rounds <- utils::read.csv("C:/Users/Erik/Desktop/Programming/R/Sports/golf/inst/extdata/golf_exports/rounds.csv") |> 
  dplyr::mutate(
    date = lubridate::parse_date_time(
      date,
      orders = c('Ymd','Y-m-d','m/d/Y','d-m-Y'),
      exact = FALSE
    ) |> as.Date(),
    date_js = as.numeric(date) * 86400000,
    course = course_name
  ) 

courses <- utils::read.csv("C:/Users/Erik/Desktop/Programming/R/Sports/golf/inst/extdata/golf_exports/courses.csv")
club_metrics <- utils::read.csv("C:/Users/Erik/Desktop/Programming/R/Sports/golf/inst/extdata/golf_exports/club_metrics.csv")

stroke_level_df <- rounds |> 
  dplyr::inner_join(
    courses |> 
      dplyr::distinct() |> 
      dplyr::select(-to_par, -slope, -course_rating, -yds, -hole_handicap)
  ) |> 
  dplyr::left_join(
    club_metrics |> dplyr::mutate(date = lubridate::parse_date_time(date, orders = c("Ymd", "Y-m-d", "m/d/Y", "d-m-Y"), exact = FALSE) |> as.Date())
  ) |> 
  dplyr::mutate(scrambling_opps = dplyr::case_when(GIR == 0 ~ 1,
                                                   TRUE ~ 0),
                scrambles = dplyr::case_when(GIR == 0 &
                                               is_gross_par == 1 ~ 1,
                                             TRUE ~ 0)) |> 
  dplyr::group_by(date) |> 
  dplyr::mutate(scramble_perc = round((sum(scrambles) / sum(scrambling_opps)) * 100, 1)) |> 
  dplyr::ungroup() |>
  dplyr::mutate(
    scrambling_perc = round(mean(scramble_perc, na.rm = TRUE), 1)
  ) |> 
  dplyr::group_by(date) |> 
  dplyr::mutate(fir_perc = dplyr::case_when(grepl(course_name, pattern = 'Dell') ~ round((tot_FIR/13)*100, 1),
                                            TRUE ~ round((tot_FIR/14)*100, 1)),
                gir_perc = round((tot_GIR/18)*100, 1)) |> 
  dplyr::ungroup()


# Page 1: Gross vs Net Score (Apexcharter) ------
# gross_series <- rounds |>
#   dplyr::distinct(
#     date, date_js, tot_gross, tot_net,
#     handicap_index, tees, course_name
#   ) |> 
#   dplyr::transmute(
#     x = date_js,
#     y = tot_gross,
#     handicap_index = as.numeric(handicap_index),
#     course_name = as.character(course_name),
#     tees = as.character(tees)
#   ) |>
#   purrr::transpose()
# 
# net_series <- rounds |>
#   dplyr::distinct(
#     date, date_js, tot_gross, tot_net,
#     handicap_index, tees, course_name
#   ) |> 
#   dplyr::transmute(
#     x = date_js,
#     y = tot_net,
#     handicap_index = as.numeric(handicap_index),
#     course_name = as.character(course_name),
#     tees = as.character(tees)
#   ) |>
#   purrr::transpose()
# 
# apexcharter::apexchart() |> 
#   apexcharter::ax_chart(type = "line") |> 
#   apexcharter::ax_colors(c("#1f77b4", "#ff7f0e")) |> 
#   apexcharter::ax_series(
#     list(name = "Gross Score", data = gross_series),
#     list(name = "Net Score", data = net_series)
#   ) |> 
#   apexcharter::ax_title(text = "Gross vs Net Score", align = "center") |> 
#   apexcharter::ax_stroke(width = c(4, 4)) |> 
#   apexcharter::ax_markers(size = 0) |> 
#   apexcharter::ax_xaxis(type = "datetime", title = list(text = 'Date')) |> 
#   apexcharter::ax_yaxis(title = list(text = "Score")) |> 
#   apexcharter::ax_tooltip(
#     shared = TRUE,
#     y = list(
#       formatter = htmlwidgets::JS(
#         "function(val, opts) {
#            const p = opts.w.config.series[opts.seriesIndex].data[opts.dataPointIndex];
#            return opts.w.globals.seriesNames[opts.seriesIndex] + ': ' + val +
#                   '<br>Handicap Index: ' + p.handicap_index +
#                   '<br>Course: ' + p.course_name +
#                   '<br>Tees: ' + p.tees;
#          }"
#       )
#     )
#   )

# Page 1 KPIs -----

# library(tidyverse)
# library(gt)
# 
# kpi_avg_gross_20 <- dataset |> 
#   dplyr::arrange(date) |> 
#   dplyr::distinct(date, tot_gross) |>
#   dplyr::slice_max(order_by = date, n = 20) |> 
#   dplyr::summarize(avg_gross = mean(tot_gross)) |> 
#   dplyr::pull(avg_gross)
# 
# kpi_avg_net_20 <- dataset |> 
#   dplyr::arrange(date) |> 
#   dplyr::distinct(date, tot_net) |> 
#   dplyr::slice_max(order_by = date, n = 20) |> 
#   dplyr::summarize(avg_net = mean(tot_net)) |> 
#   dplyr::pull(avg_net)
# 
# kpi_fir <- dataset |> 
#   dplyr::arrange(date) |> 
#   dplyr::distinct(date, tot_FIR) |> 
#   dplyr::mutate(
#     fir_perc = round(mean(tot_FIR / 14, na.rm = TRUE) * 100, 1)
#   ) |> 
#   dplyr::distinct(fir_perc) |> 
#   dplyr::pull()
# 
# kpi_gir <- dataset |> 
#   dplyr::arrange(date) |> 
#   dplyr::distinct(date, tot_GIR) |> 
#   dplyr::mutate(
#     gir_perc = round(mean(tot_GIR / 18, na.rm = TRUE) * 100, 1)
#   ) |> 
#   dplyr::distinct(gir_perc) |> 
#   dplyr::pull()
# 
# kpi_putts <- dataset |> 
#   dplyr::arrange(date) |> 
#   dplyr::distinct(date, tot_putts) |> 
#   dplyr::mutate(
#     avg_tot_putts = round(mean(tot_putts)*100, 1)
#   ) |> 
#   dplyr::distinct(avg_tot_putts) |> 
#   dplyr::pull()
# 
# kpi_scrambling <- dataset |> 
#   dplyr::mutate(
#     scrambling_opps = dplyr::case_when(GIR == 0 ~ 1, TRUE ~ 0),
#     scrambles = dplyr::case_when(GIR == 0 & is_gross_par == 1 ~ 1, TRUE ~ 0)
#   ) |> 
#   dplyr::group_by(date) |> 
#   dplyr::summarize(
#     scramble_perc = round((sum(scrambles) / sum(scrambling_opps)) * 100, 1)
#   ) |> 
#   dplyr::ungroup() |> 
#   dplyr::mutate(
#     scrambling_perc = round(mean(scramble_perc, na.rm = TRUE), 1)
#   ) |> 
#   dplyr::distinct(scrambling_perc) |> 
#   dplyr::pull()
# 
# # Handicap Index card
# hi_table <- tibble::tibble(
#   `Handicap Index` = round(
#     dataset |> 
#       dplyr::select(handicap_index) |> 
#       dplyr::slice_tail() |> 
#       dplyr::pull(),
#     1
#   )
# )
# 
# hi_table |>
#   gt::gt() |>
#   gt::tab_options(
#     table.width = gt::pct(20),
#     data_row.padding = gt::px(6),
#     table.font.size = gt::px(18)
#   ) |>
#   gt::tab_style(
#     style = gt::cell_text(weight = "bold"),
#     locations = gt::cells_column_labels()
#   ) |>
#   gt::tab_style(
#     style = gt::cell_text(align = "center"),
#     locations = gt::cells_body()
#   )
# 
# # KPI banner
# kpi_table <- tibble::tibble(
#   `Avg Gross (20)` = round(kpi_avg_gross_20, 1),
#   `Avg Net (20)`   = round(kpi_avg_net_20, 1),
#   `Avg FIR %`      = kpi_fir,
#   `Avg GIR %`      = kpi_gir,
#   `Avg Tot Putts`  = kpi_putts,
#   `Scramble %`     = kpi_scrambling
# )
# 
# kpi_table |>
#   gt::gt() |>
#   gt::tab_options(
#     table.width = gt::pct(100),
#     data_row.padding = gt::px(6),
#     table.font.size = gt::px(16)
#   ) |>
#   gt::tab_style(
#     style = gt::cell_text(weight = "bold"),
#     locations = gt::cells_column_labels()
#   )

library(tidyverse)

kpi_last20 <- stroke_level_df |>
  dplyr::arrange(date) |> 
  dplyr::transmute(
    date = as.Date(date),
    tot_gross,
    tot_net,
    tot_FIR,
    tot_GIR,
    tot_putts,
    scrambling_perc
  ) |>
  dplyr::arrange(date) |>
  dplyr::distinct(date, .keep_all = TRUE) |>
  dplyr::slice_max(order_by = date, n = 20)

kpi_values <- kpi_last20 |> 
  dplyr::mutate(date = as.Date(date)) |> 
  dplyr::select(-date) %>%
  dplyr::mutate(tot_gross = round(mean(tot_gross, na.rm = T), 1),
                tot_net = round(mean(tot_net, na.rm = T), 1),
                tot_FIR = round(mean(tot_FIR / 14, na.rm = T)*100, 1),
                tot_GIR = round(mean(tot_GIR / 18, na.rm = T)*100, 1),
                tot_putts = round(mean(tot_putts, na.rm = T), 1),
                scrambling_perc = round(mean(scrambling_perc, na.rm = T), 1)) |> 
  dplyr::distinct() |> 
  tidyr::pivot_longer(cols = dplyr::everything(), names_to = 'Metric', values_to = 'Value') |> 
  dplyr::mutate(Metric = dplyr::case_when(grepl(Metric, pattern = 'gross') ~ 'Avg Gross\n(20)',
                                          grepl(Metric, pattern = 'net') ~ 'Avg Net\n(20)',
                                          grepl(Metric, pattern = 'FIR') ~ 'Avg FIR %',
                                          grepl(Metric, pattern = 'GIR') ~ 'Avg GIR %',
                                          grepl(Metric, pattern = 'putts') ~ 'Avg Tot. Putts',
                                          grepl(Metric, pattern = 'scramb') ~ 'Avg Scramble %'))


n <- nrow(kpi_values)
xpos <- seq(1, n)  # inline numeric positions

ggplot2::ggplot() +
  ggplot2::geom_tile(
    ggplot2::aes(x = mean(xpos), y = 1),
    fill = "navy",
    width = n,
    height = 1.2
  ) +
  ggplot2::annotate(
    "text",
    x = xpos,
    y = 1.15,
    label = kpi_values$Value,
    color = "white",
    size = 7,
    fontface = "bold",
    hjust = 0.5
  ) +
  ggplot2::annotate(
    "text",
    x = xpos,
    y = 0.85,
    label = kpi_values$Metric,
    colour = "white",
    size = 6,
    hjust = 0.5
  ) +
  ggplot2::scale_x_continuous(limits = c(0.5, n + 0.5), expand = c(0, 0)) +
  ggplot2::scale_y_continuous(limits = c(0.4, 1.6), expand = c(0, 0)) +
  ggplot2::theme_void()




# Page 2 — Round -level Summaries -----

# rounds <- utils::read.csv("C:/Users/Erik/Desktop/Programming/R/Sports/golf/inst/extdata/golf_exports/rounds.csv")
# courses <- utils::read.csv("C:/Users/Erik/Desktop/Programming/R/Sports/golf/inst/extdata/golf_exports/courses.csv")
# 
# rounds <- rounds |>
#   dplyr::mutate(
#     date = lubridate::parse_date_time(
#       date,
#       orders = c("Ymd", "Y-m-d", "m/d/Y", "d-m-Y"),
#       exact = FALSE
#     ) |> as.Date(),
#     date_js = as.numeric(date) * 86400000,
#     course = course_name
#   )

# table
# library(dplyr)
# library(ggpubr)

# ggplot2::ggplot(
#   stroke_level_df |>
#     dplyr::group_by(date, course_name, tees, tot_gross, tot_net, scrambling_perc) |>
#     dplyr::summarize(
#       fir_perc = round(mean(tot_FIR / 14, na.rm = TRUE) * 100, 1),
#       gir_perc = round(mean(tot_GIR / 18, na.rm = TRUE) * 100, 1),
#       tot_putts = round(mean(tot_putts, na.rm = TRUE), 1),
#       tot_penalties = round(mean(tot_penalties, na.rm = TRUE), 1)
#     ) |>
#     dplyr::rename(
#       Date = date,
#       Course = course_name,
#       Tees = tees,
#       `Total Gross` = tot_gross,
#       `Total Net` = tot_net,
#       `FIR %` = fir_perc,
#       `GIR %` = gir_perc,
#       `Scramble %` = scrambling_perc,
#       `Total Putts` = tot_putts,
#       `Total Penalties` = tot_penalties
#     ) |>
#     dplyr::ungroup() |>
#     dplyr::mutate(Date = as.character(Date)) |> 
#     dplyr::mutate(Round = paste(Course, Date, sep = ' '), .before = 1) |> 
#     dplyr::select(-Date, -Course) |> 
#     dplyr::mutate(dplyr::across(dplyr::everything(), as.character)) |>
#     tidyr::pivot_longer(
#       # cols = -Date,
#       cols = dplyr::everything(),
#       names_to = "colname",
#       values_to = "value"
#     ) |>
#     dplyr::mutate(
#       row = dplyr::case_when(colname == 'Round' ~ 1, TRUE ~ NA)
#       ) |> 
#     dplyr::mutate(col = match(colname, unique(colname))) |> 
#     dplyr::group_by(colname) |> 
#     dplyr::mutate(row = cumsum(row == 1)) |> 
#     dplyr::ungroup() |> 
#     tidyr::fill(row, .direction = 'down')
# ) +
#   ggplot2::geom_tile(
#     ggplot2::aes(x = col, y = row),
#     fill = "white",
#     color = "black",
#     linewidth = 0.3
#   ) +
#   ggplot2::geom_text(
#     ggplot2::aes(x = col, y = row, label = value),
#     size = 3.5
#   ) +
#   ggplot2::geom_text(
#     ggplot2::aes(
#       x = col,
#       y = max(row) + 1,
#       label = colname
#     ),
#     fontface = "bold",
#     color = 'black',
#     size = 4
#   ) +
#   ggplot2::theme_void()



# round_table <- rounds |>
#   dplyr::mutate(
#     scrambling_opps = dplyr::case_when(GIR == 0 ~ 1, TRUE ~ 0),
#     scrambles = dplyr::case_when(GIR == 0 & is_gross_par == 1 ~ 1, TRUE ~ 0)
#   ) |>
#   dplyr::group_by(date) |>
#   dplyr::mutate(
#     scramble_perc = round((sum(scrambles) / sum(scrambling_opps)) * 100, 1)
#   ) |>
#   dplyr::mutate(
#     fir_perc = round(mean(tot_FIR / 14, na.rm = TRUE) * 100, 1),
#     gir_perc = round(mean(tot_GIR / 18, na.rm = TRUE) * 100, 1)
#   ) |>
#   dplyr::ungroup() |>
#   dplyr::mutate(
#     scrambling_perc = round(mean(scramble_perc, na.rm = TRUE), 1)
#   ) |>
#   dplyr::distinct(
#     date, course_name, tees, tot_gross, tot_net,
#     fir_perc, gir_perc, scramble_perc, tot_putts, tot_penalties
#   ) |>
#   dplyr::rename(
#     Date = date,
#     Course = course_name,
#     Tees = tees,
#     `Gross Score` = tot_gross,
#     `Net Score` = tot_net,
#     `FIR %` = fir_perc,
#     `GIR %` = gir_perc,
#     `Scramble %` = scramble_perc,
#     Putts = tot_putts,
#     Penalties = tot_penalties
#   )
# 
# p_round_table <- round_table |>
#   gt::gt() |>
#   gt::tab_options(
#     table.width = gt::pct(100),
#     data_row.padding = gt::px(4),
#     table.font.size = gt::px(14)
#   ) |>
#   gt::tab_style(
#     style = gt::cell_text(weight = "bold"),
#     locations = gt::cells_column_labels()
#   )
# 
# print(p_round_table)

# Global ggplot theme
ggplot2::theme_set(
  ggplot2::theme_bw(base_size = 12) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 14),
      axis.title = ggplot2::element_text(face = "bold", size = 12),
      axis.text = ggplot2::element_text(face = "bold", size = 11),
      strip.background = ggplot2::element_rect(fill = "white", color = "black"),
      strip.text = ggplot2::element_text(face = "bold", size = 12),
      legend.position = "none"
    )
)

# GIR vs Gross 
stroke_level_df |>
  dplyr::distinct(date, course_name, tot_gross, gir_perc) |>
  dplyr::filter(!is.na(gir_perc)) |>
  ggplot2::ggplot(
    ggplot2::aes(
      x = gir_perc,
      y = tot_gross,
      color = course_name,
      fill = course_name
    )
  ) +
  ggplot2::geom_point(size = 3, alpha = 0.7) +
  ggplot2::labs(
    title = "Gross Score vs GIR %",
    x = "GIR %",
    y = "Gross Score",
    legend = "Course"
  ) +
  ggplot2::guides(fill = "none", color = "none") +
  ggplot2::geom_smooth(method = "lm") +
  ggplot2::facet_wrap(~course_name)


# FIR vs Gross
stroke_level_df |> 
  dplyr::relocate(par, .after = hole) |>
  dplyr::group_by(date, course_name) |>
  dplyr::mutate(
    fir_opps = dplyr::case_when(par > 3 ~ 1, TRUE ~ 0),
    .after = par
  ) |>
  dplyr::filter(fir_opps == 1) |>
  dplyr::group_by(date, course_name, tee_club) |>
  dplyr::mutate(
    fir_perc_club = round(
      (sum(FIR, na.rm = TRUE) / sum(fir_opps, na.rm = TRUE)) * 100,
      1
    ),
    .after = fir_opps
  ) |>
  dplyr::group_by(date, course_name, tees, handicap_index) |>
  dplyr::mutate(
    fir_perc = round(mean(fir_perc_club, na.rm = TRUE), 1)
  ) |>
  dplyr::ungroup() |>
  dplyr::select(
    date, course_name, tees, handicap_index,
    tot_gross, fir_perc_club, fir_perc, tee_club
  ) |>
  dplyr::filter(grepl(tee_club, pattern = "4|5|3W|D")) |>
  dplyr::distinct(
    date, course_name, tees, handicap_index,
    tot_gross, fir_perc_club, fir_perc, tee_club
  ) |>
  ggplot2::ggplot(
    ggplot2::aes(
      x = fir_perc_club,
      y = tot_gross,
      color = tee_club,
      fill = tee_club
    )
  ) +
  ggplot2::geom_point(size = 3, alpha = 0.7) +
  ggplot2::labs(
    title = "Gross Score vs FIR %",
    x = "FIR %",
    y = "Gross Score"
  ) +
  ggplot2::guides(fill = "none", color = "none") +
  ggplot2::geom_smooth(method = "lm") +
  ggplot2::facet_wrap(~factor(tee_club, levels = c("4", "3W", "D")))

# Putts vs Gross
stroke_level_df |>
  dplyr::distinct(date, course_name, tot_putts, tot_gross) |>
  dplyr::filter(!is.na(tot_putts)) |>
  ggplot2::ggplot(
    ggplot2::aes(
      x = tot_putts,
      y = tot_gross,
      color = course_name,
      fill = course_name
    )
  ) +
  ggplot2::geom_point(size = 3, alpha = 0.7) +
  ggplot2::labs(
    title = "Gross Score vs Total Putts",
    x = "# of Putts",
    y = "Gross Score",
    legend = "Course"
  ) +
  ggplot2::guides(fill = "none", color = "none") +
  ggplot2::geom_smooth(method = "lm") +
  ggplot2::facet_wrap(~course_name)

# Penalties vs Gross
stroke_level_df |>
  dplyr::distinct(date, course_name, tot_penalties, tot_gross) |>
  dplyr::filter(!is.na(tot_penalties)) |>
  ggplot2::ggplot(
    ggplot2::aes(
      x = tot_penalties,
      y = tot_gross,
      color = course_name,
      fill = course_name
    )
  ) +
  ggplot2::geom_point(size = 3, alpha = 0.7) +
  ggplot2::labs(
    title = "Gross Score vs Total Penalties",
    x = "# of Penalties",
    y = "Gross Score",
    legend = "Course"
  ) +
  ggplot2::guides(fill = "none", color = "none") +
  ggplot2::geom_smooth(method = "lm") +
  ggplot2::facet_wrap(~course_name)


# Scramble % vs Gross
stroke_level_df |> 
  dplyr::distinct(date, course_name, tees, tot_gross, scramble_perc) |>
  dplyr::filter(!is.na(scramble_perc)) |>
  ggplot2::ggplot(
    ggplot2::aes(
      x = scramble_perc,
      y = tot_gross,
      color = course_name,
      fill = course_name
    )
  ) +
  ggplot2::geom_point(size = 3, alpha = 0.7) +
  ggplot2::labs(
    title = "Gross Score vs Scramble %",
    x = "Up And Down %",
    y = "Gross Score",
    legend = "Course"
  ) +
  ggplot2::guides(fill = "none", color = "none") +
  ggplot2::geom_smooth(method = "lm") +
  ggplot2::facet_wrap(~course_name)

# page 3 skill decomposition ----
club_metrics <- utils::read.csv("C:/Users/Erik/Desktop/Programming/R/Sports/golf/inst/extdata/golf_exports/club_metrics.csv")

## par-3 GIR % by tees
stroke_level_df |> 
  dplyr::relocate(par, .after = hole) |> 
  dplyr::group_by(date, course_name) |> 
  dplyr::mutate(gir_opps = dplyr::case_when(par == 3 ~ 1, TRUE ~ 0), .after = par) |> 
  dplyr::filter(gir_opps == 1) |> 
  dplyr::group_by(date, course_name) |> 
  dplyr::mutate(gir_perc = round((sum(GIR, na.rm = TRUE) / sum(gir_opps, na.rm = TRUE))*100, 1)) |> 
  dplyr::ungroup() |> 
  dplyr::select(date, course_name, tees, handicap_index, tot_gross, gir_perc) |>  
  dplyr::distinct(date, course_name, tees, handicap_index, tot_gross, gir_perc) |> 
  dplyr::filter(!grepl(course_name, pattern = "National|Sew")) |> 
  ggplot2::ggplot(ggplot2::aes(x = gir_perc, y = tot_gross, color = tees, fill = tees)) +
  ggplot2::geom_point(size = 3, alpha = 0.7) +
  ggplot2::labs(title = "Gross Score vs Par-3 GIR % by Tee Selection",
                x = "GIR %",
                y = "Gross Score"
  ) +
  ggplot2::guides(fill = "none", color = "none") +
  ggplot2::geom_smooth(method = "lm") +
  ggplot2::facet_wrap(~ tees)

## par-3 GIR % by course
stroke_level_df |>
  dplyr::relocate(par, .after = hole) |> 
  dplyr::group_by(date, course_name) |> 
  dplyr::mutate(gir_opps = dplyr::case_when(par == 3 ~ 1, TRUE ~ 0), .after = par) |> 
  dplyr::filter(gir_opps == 1) |> 
  dplyr::group_by(date, course_name) |> 
  dplyr::mutate(gir_perc = round((sum(GIR, na.rm = TRUE) / sum(gir_opps, na.rm = TRUE))*100, 1)) |> 
  dplyr::ungroup() |> 
  dplyr::select(date, course_name, tees, handicap_index, tot_gross, gir_perc) |>  
  dplyr::distinct(date, course_name, tees, handicap_index, tot_gross, gir_perc) |> 
  dplyr::filter(!grepl(course_name, pattern = "National|Sew")) |> 
  ggplot2::ggplot(ggplot2::aes(x = gir_perc, y = tot_gross, color = course_name, fill = course_name)) +
  ggplot2::geom_point(size = 3, alpha = 0.7) +
  ggplot2::labs(title = "Gross Score vs Par-3 GIR % by Course",
                x = "GIR %",
                y = "Gross Score"
  ) +
  ggplot2::guides(fill = "none", color = "none") +
  ggplot2::geom_smooth(method = "lm") +
  ggplot2::facet_wrap(~ course_name)

## options approach GIR % by lie, course, club, tee, par
# overall 
stroke_level_df |> 
  dplyr::mutate(approach_shots = dplyr::case_when(par == 3 & 
                                                    stroke == 1 ~ 1,
                                                  par > 3 & 
                                                    shot_type == "full" &
                                                    yds_to_target > 75 &
                                                    club != "D" &
                                                    !grepl(lie, pattern = "bunker|tee") ~ 1,
                                                  TRUE ~ 0)) |> 
  dplyr::filter(approach_shots == 1) |> 
  dplyr::mutate(approach_gir = GIR) |> 
  dplyr::group_by(yds_to_target, club) |> 
  dplyr::mutate(approach_perc = round(mean(approach_gir, na.rm = TRUE)*100, 1)) |> 
  ggplot2::ggplot(ggplot2::aes(y = approach_perc, x = yds_to_target)) +
  ggplot2::geom_point(size = 3, alpha = 0.7) +
  ggplot2::labs(title = "Approach Distance vs GIR Probability",
                y = "Probability of GIR %",
                x = "Distance to Target (yds)"
  ) +
  ggplot2::guides(fill = "none", color = "none") +
  ggplot2::geom_smooth(method = "loess")

# club
stroke_level_df |> 
  dplyr::mutate(approach_shots = dplyr::case_when(par == 3 & 
                                                    stroke == 1 ~ 1,
                                                  par > 3 & 
                                                    shot_type == "full" &
                                                    yds_to_target > 75 &
                                                    club != "D" &
                                                    !grepl(lie, pattern = "bunker|tee") ~ 1,
                                                  TRUE ~ 0)) |> 
  dplyr::filter(approach_shots == 1) |> 
  dplyr::mutate(approach_gir = GIR) |> 
  dplyr::group_by(yds_to_target, club) |> 
  dplyr::mutate(approach_perc = round(mean(approach_gir, na.rm = TRUE)*100, 1)) |> 
  ggplot2::ggplot(ggplot2::aes(y = approach_perc, x = yds_to_target, color = club, fill = club)) +
  ggplot2::geom_point(size = 3, alpha = 0.7) +
  ggplot2::labs(title = "Approach GIR Probability vs Distance (by Club)",
                y = "Probability of GIR %",
                x = "Distance to Target (yds)"
  ) +
  ggplot2::guides(fill = "none", color = "none") +
  ggplot2::geom_smooth(method = "loess") +
  ggplot2::facet_wrap(~ factor(club, levels = c("3W", "4", "5", "6", "7", "8", "9", "PW", "GW", "SW", "LW")), ncol = 3, scales = "free_y")

# lie
stroke_level_df |> 
  dplyr::mutate(approach_shots = dplyr::case_when(par == 3 & 
                                                    stroke == 1 ~ 1,
                                                  par > 3 & 
                                                    shot_type == "full" &
                                                    yds_to_target > 75 &
                                                    club != "D" &
                                                    !grepl(lie, pattern = "bunker|tee") ~ 1,
                                                  TRUE ~ 0)) |> 
  dplyr::filter(approach_shots == 1) |> 
  dplyr::mutate(approach_gir = GIR) |> 
  dplyr::group_by(yds_to_target, lie) |> 
  dplyr::mutate(approach_perc = round(mean(approach_gir, na.rm = TRUE)*100, 1)) |> 
  ggplot2::ggplot(ggplot2::aes(y = approach_perc, x = yds_to_target, color = lie, fill = lie)) +
  ggplot2::geom_point(size = 3, alpha = 0.7) +
  ggplot2::labs(title = "Approach GIR Probability vs Distance (by Lie)",
                y = "Probability of GIR %",
                x = "Distance to Target (yds)"
  ) +
  ggplot2::guides(fill = "none", color = "none") +
  ggplot2::geom_smooth(method = "loess") +
  ggplot2::facet_wrap(~ lie)

# page 4 club & shot type diagnostics ----

stroke_quality <- stroke_level_df |> 
  dplyr::mutate(dplyr::across(dplyr::contains("yds"), ~as.numeric(.x))) |> 
  dplyr::mutate(
    yd_diff = yds_to_target - yds_traveled,
    on_target = dplyr::case_when(on_target == "yes" ~ 1, TRUE ~ 0)
  )

## ignoring miss direction
full_stroke_quality_avg <- stroke_quality |>
  dplyr::filter(grepl(shot_type, pattern = "full|tee")) |>
  dplyr::group_by(club) |>
  dplyr::summarize(
    avg_yds_to_target = round(mean(yds_to_target), 1),
    avg_yds_traveled = round(mean(yds_traveled), 1),
    sd_yds_traveled = round(sd(yds_traveled), 1),
    avg_yd_diff = round(mean(yd_diff), 1),
    avg_accuracy = round((sum(on_target)/dplyr::n())*100, 2),
    sd_accuracy = round((sd(on_target)/dplyr::n())*100, 2),
    n = dplyr::n()
  ) |>
  dplyr::rename(`club strokes` = n)

## calibration window
cal_windows <- tibble::tribble(
  ~club, ~ymin, ~ymax,
  "D", 260, 280,
  "3W", 235, 255,
  "4", 200, 220,
  "5", 190, 200,
  "6", 180, 190,
  "7", 170, 180,
  "8", 160, 170,
  "9", 150, 160,
  "PW", 140, 150,
  "GW", 110, 130,
  "SW", 50, 100
)

## shot distance window by club
full_stroke_quality_avg |> 
  dplyr::ungroup() |> 
  dplyr::mutate(
    club = factor(
      club,
      levels = c('LW', 'SW', 'GW', 'PW', '9', '8', '7', '6', '5', '4', '3W', 'D')
    ),
    x = as.numeric(club)
  ) |> 
  dplyr::left_join(cal_windows, by = 'club') |> 
  ggplot2::ggplot(ggplot2::aes(
    x = x,
    y = avg_yds_traveled,
    color = club,
    fill = club
  )) +
  ggplot2::geom_rect(
    ggplot2::aes(
      xmin = x - 0.45,
      xmax = x + 0.45,
      ymin = ymin,
      ymax = ymax
    ),
    fill = 'black',
    alpha = 0.30,
    inherit.aes = FALSE
  ) + 
  ggplot2::geom_point(size = 3) +
  ggplot2::geom_errorbar(
    ggplot2::aes(
      x = x,
      ymin = avg_yds_traveled - sd_yds_traveled,
      ymax = avg_yds_traveled + sd_yds_traveled,
      color = club,
      fill = club
    ),
    inherit.aes = FALSE,
    width = 0.15,
    linewidth = 0.7
  ) +
  ggplot2::scale_x_continuous(
    breaks = seq_along(c('LW','SW','GW','PW','9','8','7','6','5','4','3W','D')),
    labels = c('LW','SW','GW','PW','9','8','7','6','5','4','3W','D'),
    minor_breaks = NULL
  ) +
  ggplot2::labs(
    title = 'Actual Shot Distance by Club (full + tee)',
    subtitle = "Shaded bands show calibrated full-swing distance windows",
    x = 'Club',
    y = 'Mean Distance (yd)',
    fill = 'none'
  ) +
  ggplot2::theme_bw(base_size = 14) +
  ggplot2::theme(
    panel.grid.minor.x = ggplot2::element_blank(),
    legend.position = 'none'
  )

## accuracy by club
stroke_quality |> 
  dplyr::filter(grepl(shot_type, pattern = 'full|tee')) |> 
  dplyr::group_by(club) |> 
  dplyr::summarize(
    accuracy = round((sum(on_target)/dplyr::n())*100, 2),
    avg_yds_traveled = round(mean(yds_traveled, na.rm = TRUE), 1),
    n = dplyr::n()
  ) |> 
  ggplot2::ggplot(
    ggplot2::aes(
      x = forcats::fct_reorder(club, avg_yds_traveled),
      y = accuracy,
      color = club,
      fill = club
    )
  ) +
  ggplot2::geom_col(alpha = 0.8) +
  ggplot2::geom_text(
    ggplot2::aes(label = paste0(accuracy, ' %')),
    color = 'black',
    vjust = -0.5,
    size = 4
  ) +
  ggplot2::labs(
    title = 'Full Stroke Accuracy by Club',
    x = 'Club',
    y = 'Accuracy %'
  ) +
  ggplot2::geom_text(
    ggplot2::aes(label = paste0("n = ", n)),
    vjust = 1.5,
    size = 3,
    color = "black"
  ) +
  ggplot2::theme_bw() +
  ggplot2::theme(
    legend.position = 'none',
    title = ggplot2::element_text(face = 'bold', size = 14),
    axis.title = ggplot2::element_text(face = 'bold', size = 12),
    axis.text = ggplot2::element_text(face = 'bold', size = 11)
  )

## approach distance distribution
stroke_level_df |>
  dplyr::mutate(
    date = lubridate::parse_date_time(
      date,
      orders = c('Ymd','Y-m-d','m/d/Y','d-m-Y'),
      exact = FALSE
    ) |> as.Date()
  ) |> 
  dplyr::mutate(
    approach_shots = dplyr::case_when(
      par == 3 & stroke == 1 ~ 1,
      par > 3 &
        shot_type == 'full' &
        yds_to_target > 75 &
        club != 'D' &
        !grepl(lie, pattern = 'bunker|tee') ~ 1,
      TRUE ~ 0
    )
  ) |> 
  dplyr::filter(approach_shots == 1) |> 
  ggplot2::ggplot(
    ggplot2::aes(
      x = yds_to_target,
      color = club,
      fill = club
    )
  ) +
  ggplot2::geom_histogram(alpha = 0.5, binwidth = 5) +
  ggplot2::guides(alpha = 'none') +
  ggplot2::labs(
    title = 'Approach Distance Distribution\n(binwidth = 5 yds)',
    y = '# of Attempts',
    x = 'Target Distance (yds)',
    fill = 'Club'
  ) +
  ggplot2::guides(color = 'none') +
  ggplot2::theme_bw() +
  ggplot2::theme(
    legend.position = 'right',
    title = ggplot2::element_text(face = 'bold', size = 12),
    axis.title = ggplot2::element_text(face = 'bold', size = 11),
    axis.text = ggplot2::element_text(face = 'bold', size = 10),
    legend.title = ggplot2::element_text(face = 'bold', size = 10),
    legend.text = ggplot2::element_text(face = 'bold', size = 10)
  )

## miss direction by club
stroke_level_df |> 
  dplyr::filter(shot_type %in% c('full','tee')) |>
  dplyr::count(club, miss_direction) |>
  dplyr::group_by(club) |> 
  dplyr::mutate(
    club = factor(
      club,
      levels = c('LW','SW','GW','PW','9','8','7','6','5','4','3W','D')
    )
  ) |> 
  dplyr::arrange(club) |> 
  dplyr::mutate(pct = round((n / sum(n))*100, 1)) |>
  dplyr::ungroup() |> 
  ggplot2::ggplot(
    ggplot2::aes(
      x = club,
      y = pct,
      fill = miss_direction
    )
  ) +
  ggplot2::geom_col(alpha = 0.85) +
  ggplot2::labs(
    title = 'Miss Direction Composition by Club',
    x = 'Club',
    y = 'Proportion of Miss Types (%)',
    fill = 'Miss\nDirection'
  ) +
  ggplot2::theme_bw(base_size = 14) +
  ggplot2::theme(
    legend.position = 'right',
    title = ggplot2::element_text(face = 'bold', size = 12),
    axis.title = ggplot2::element_text(face = 'bold', size = 11),
    axis.text = ggplot2::element_text(face = 'bold', size = 10),
    legend.title = ggplot2::element_text(face = 'bold', size = 10),
    legend.text = ggplot2::element_text(face = 'bold', size = 10)
  )

## distance control (relative to calibration)
full_stroke_quality_avg |>
  dplyr::ungroup() |>
  dplyr::mutate(
    club = factor(
      club,
      levels = c('LW','SW','GW','PW','9','8','7','6','5','4','3W','D')
    ),
    x = as.numeric(club)
  ) |>
  dplyr::left_join(cal_windows, by = 'club') |>
  dplyr::mutate(
    target_center  = (ymin + ymax) / 2,
    distance_error = avg_yds_traveled - target_center
  ) |>
  ggplot2::ggplot(
    ggplot2::aes(
      x     = x,
      y     = distance_error,
      color = club
    )
  ) +
  ggplot2::geom_point(size = 3) +
  ggplot2::geom_errorbar(
    ggplot2::aes(
      ymin = (avg_yds_traveled - sd_yds_traveled) - target_center,
      ymax = (avg_yds_traveled + sd_yds_traveled) - target_center
    ),
    width     = 0.15,
    linewidth = 0.7
  ) +
  ggplot2::geom_hline(
    yintercept = 0,
    linewidth  = 0.7,
    linetype   = 'dashed',
    color      = 'gray40'
  ) +
  ggplot2::scale_x_continuous(
    breaks       = seq_along(c('LW','SW','GW','PW','9','8','7','6','5','4','3W','D')),
    labels       = c('LW','SW','GW','PW','9','8','7','6','5','4','3W','D'),
    minor_breaks = NULL
  ) +
  ggplot2::labs(
    title    = 'Distance Control by Club',
    subtitle = 'Error = Actual Distance − Calibrated Distance',
    x        = 'Club',
    y        = 'Distance Error (yd)'
  ) +
  ggplot2::theme_bw(base_size = 14) +
  ggplot2::theme(
    title = ggplot2::element_text(face = 'bold', size = 12),
    axis.title = ggplot2::element_text(face = 'bold', size = 11),
    axis.text = ggplot2::element_text(face = 'bold', size = 10),
    legend.position = 'none',
    panel.grid.major.x = ggplot2::element_blank(),
    panel.grid.minor.x = ggplot2::element_blank()
  )

## distance control
full_stroke_quality_avg |>
  dplyr::ungroup() |>
  dplyr::mutate(
    club = factor(
      club,
      levels = c('LW','SW','GW','PW','9','8','7','6','5','4','3W','D')
    ),
    x = as.numeric(club),
    distance_error = avg_yds_traveled - avg_yds_to_target
  ) |>
  ggplot2::ggplot(
    ggplot2::aes(
      x     = x,
      y     = distance_error,
      color = club
    )
  ) +
  ggplot2::geom_point(size = 3) +
  ggplot2::geom_errorbar(
    ggplot2::aes(
      ymin = (avg_yds_traveled - sd_yds_traveled) - avg_yds_to_target,
      ymax = (avg_yds_traveled + sd_yds_traveled) - avg_yds_to_target
    ),
    width     = 0.15,
    linewidth = 0.7
  ) +
  ggplot2::geom_hline(
    yintercept = 0,
    linewidth  = 0.7,
    linetype   = 'dashed',
    color      = 'gray40'
  ) +
  ggplot2::scale_x_continuous(
    breaks       = seq_along(c('LW','SW','GW','PW','9','8','7','6','5','4','3W','D')),
    labels       = c('LW','SW','GW','PW','9','8','7','6','5','4','3W','D'),
    minor_breaks = NULL
  ) +
  ggplot2::labs(
    title    = 'Distance Control by Club',
    subtitle = 'Error = Actual Distance − Target Distance',
    x        = 'Club',
    y        = 'Distance Error (yd)'
  ) +
  ggplot2::theme_bw(base_size = 14) +
  ggplot2::theme(
    title = ggplot2::element_text(face = 'bold', size = 12),
    axis.title = ggplot2::element_text(face = 'bold', size = 11),
    axis.text = ggplot2::element_text(face = 'bold', size = 10),
    legend.position = 'none',
    panel.grid.major.x = ggplot2::element_blank(),
    panel.grid.minor.x = ggplot2::element_blank()
  )

# page 5 lie & contact condition ----
## distance lost by lie
full_stroke_quality_w_lie <- stroke_quality |>
  dplyr::filter(grepl(shot_type, pattern = 'full|tee')) |>
  dplyr::group_by(club, lie) |>
  dplyr::summarize(
    avg_yds_to_target = round(mean(yds_to_target, na.rm = T), 1),
    avg_yds_traveled  = round(mean(yds_traveled, na.rm = T), 1),
    sd_yds_traveled   = round(sd(yds_traveled, na.rm = T), 1),
    avg_yd_diff       = round(mean(yd_diff, na.rm = T), 1),
    avg_accuracy      = round((sum(on_target, na.rm = T) / dplyr::n()) * 100, 2),
    sd_accuracy       = round((sd(on_target, na.rm = T) / dplyr::n()) * 100, 2),
    n                 = dplyr::n()
  ) |>
  dplyr::rename(`club strokes` = n)

full_stroke_quality_w_lie |>
  dplyr::ungroup() |>
  dplyr::mutate(
    lie = factor(
      lie,
      levels = c('tee', 'fairway', 'rough', 'fwbunker')
    ),
    distance_loss = avg_yds_to_target - avg_yds_traveled
  ) |>
  ggplot2::ggplot(
    ggplot2::aes(
      x    = lie,
      y    = distance_loss,
      fill = lie
    )
  ) +
  ggplot2::geom_boxplot(
    alpha        = 0.6,
    outlier.alpha = 0.4,
    width        = 0.7
  ) +
  ggplot2::geom_hline(
    yintercept = 0,
    linewidth  = 0.7,
    linetype   = 'dashed',
    color      = 'gray40'
  ) +
  ggplot2::labs(
    title    = 'Distance Loss by Lie Type',
    subtitle = 'target − actual distance',
    x        = 'Lie Type',
    y        = 'Distance Loss (yd)'
  ) +
  ggplot2::theme_bw(base_size = 14) +
  ggplot2::theme(
    title = ggplot2::element_text(face = 'bold', size = 12),
    axis.title = ggplot2::element_text(face = 'bold', size = 11),
    axis.text = ggplot2::element_text(face = 'bold', size = 10),
    panel.grid.major.x = ggplot2::element_blank(),
    panel.grid.minor.x = ggplot2::element_blank()
  )

## miss direction by lie type
full_stroke_quality_w_lie_miss <- stroke_quality |>
  dplyr::filter(grepl(shot_type, pattern = 'full|tee')) |>
  dplyr::group_by(club, lie, miss_direction) |>
  dplyr::summarize(
    avg_yds_to_target = round(mean(yds_to_target), 1),
    avg_yds_traveled  = round(mean(yds_traveled), 1),
    sd_yds_traveled   = round(sd(yds_traveled), 1),
    avg_yd_diff       = round(mean(yd_diff), 1),
    avg_accuracy      = round((sum(on_target) / dplyr::n()) * 100, 2),
    sd_accuracy       = round((sd(on_target) / dplyr::n()) * 100, 2),
    n                 = dplyr::n()
  ) |>
  dplyr::rename(`club strokes` = n)

full_stroke_quality_w_lie_miss |>
  dplyr::ungroup() |>
  dplyr::filter(
    lie %in% c('tee', 'fairway', 'rough', 'fwbunker')
  ) |>
  dplyr::mutate(
    lie = factor(
      lie,
      levels = c('tee', 'fairway', 'rough', 'fwbunker')
    ),
    miss_direction = factor(
      miss_direction,
      levels = c('left', 'right', 'short', 'long', 'on_target')
    )
  ) |>
  ggplot2::ggplot(
    ggplot2::aes(
      x    = lie,
      fill = miss_direction
    )
  ) +
  ggplot2::geom_bar(
    position = 'fill',
    alpha    = 0.85
  ) +
  ggplot2::scale_y_continuous(
    labels       = scales::percent_format(accuracy = 1),
    minor_breaks = NULL
  ) +
  ggplot2::labs(
    title    = 'Miss Direction Composition by Lie Type',
    subtitle = 'Proportion of left/right/short/long/on-target outcomes',
    x        = 'Lie Type',
    y        = 'Proportion of Shots (%)',
    fill     = 'Miss\nDirection'
  ) +
  ggplot2::theme_bw(base_size = 14) +
  ggplot2::theme(
    title = ggplot2::element_text(face = 'bold', size = 12),
    axis.title = ggplot2::element_text(face = 'bold', size = 11),
    axis.text = ggplot2::element_text(face = 'bold', size = 10),
    panel.grid.major.x = ggplot2::element_blank(),
    panel.grid.minor.x = ggplot2::element_blank()
  )


## miss direction by lie type and club
full_stroke_quality_w_lie_miss |>
  dplyr::ungroup() |>
  dplyr::filter(
    lie %in% c('tee', 'fairway', 'rough', 'fwbunker')
  ) |>
  dplyr::mutate(
    lie = factor(
      lie,
      levels = c('tee', 'fairway', 'rough', 'fwbunker')
    ),
    miss_direction = factor(
      miss_direction,
      levels = c('left', 'right', 'short', 'long', 'on_target')
    ),
    club = factor(
      club,
      levels = c('LW','SW','GW','PW','9','8','7','6','5','4','3W','D')
    )
  ) |>
  ggplot2::ggplot(
    ggplot2::aes(
      x    = club,
      fill = miss_direction
    )
  ) +
  ggplot2::geom_bar(
    position = 'fill',
    alpha    = 0.85
  ) +
  ggplot2::scale_y_continuous(
    labels       = scales::percent_format(accuracy = 1),
    minor_breaks = NULL
  ) +
  ggplot2::facet_wrap(~ lie, nrow = 2) +
  ggplot2::labs(
    title    = 'Miss-Direction Composition by Club × Lie Type',
    subtitle = 'Proportion of left/right/short/long/on-target outcomes',
    x        = 'Club',
    y        = 'Proportion of Shots (%)',
    fill     = 'Miss\nDirection'
  ) +
  ggplot2::theme_bw(base_size = 14) +
  ggplot2::theme(
    title = ggplot2::element_text(face = 'bold', size = 12),
    axis.title = ggplot2::element_text(face = 'bold', size = 11),
    axis.text = ggplot2::element_text(face = 'bold', size = 10),
    panel.grid.major.x = ggplot2::element_blank(),
    panel.grid.minor.x = ggplot2::element_blank(),
    strip.background   = ggplot2::element_rect(fill = 'white', color = 'black')
  )
