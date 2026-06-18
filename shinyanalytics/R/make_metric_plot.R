# metric plots ----
# make_metric_plot <- function(df, metric) {
#   if (!metric %in% names(df)) {
#     stop("Column not found: ", metric)
#   }
#   
#   label_map <- c(
#     fir       = "FIR %",
#     gir       = "GIR %",
#     updown   = "Up & Down %",
#     total_putts     = "Putts per Round",
#     total_penalties = "Penalties per Round"
#   )
#   
#   ylab <- label_map[[metric]]
#   
#   df2 <- df |>
#     dplyr::arrange(date) |>
#     dplyr::select(date, value = dplyr::all_of(metric))
#   
#   ggplot2::ggplot(
#     df2,
#     ggplot2::aes(x = date, y = value)
#   ) +
#     ggplot2::geom_point(size = 3, alpha = 0.8) +
#     ggplot2::geom_smooth(method = "lm", linewidth = 1.2) +
#     ggplot2::labs(
#       title = metric,
#       x = "Date",
#       y = metric
#     )
# }

# make_metric_plot <- function(df,
#                              x,
#                              y,
#                              facet_var = NULL,
#                              smooth = FALSE,
#                              flip_axes = FALSE,
#                              xlab = NULL,
#                              ylab = NULL) {
#   
#   if (!x %in% names(df)) stop("Column not found: ", x)
#   if (!y %in% names(df)) stop("Column not found: ", y)
#   
#   if (is.null(facet_var)) {
#     df2 <- df |>
#       purrr::transmute(
#         xval = .data[[x]],
#         yval = .data[[y]]
#       )
#   } else {
#     df2 <- df |>
#       purrr::transmute(
#         xval = .data[[x]],
#         yval = .data[[y]],
#         facet = .data[[facet_var]]
#       )
#   }
#   
#   p <- ggplot2::ggplot(df2, ggplot2::aes(x = xval, y = yval)) +
#     ggplot2::geom_point(size = 3, alpha = 0.8)
#   
#   if (inherits(df2$xval, "Date")) {
#     p <- p + ggplot2::scale_x_date(
#       date_breaks = '2 weeks',
#       date_labels = '%b %d'
#     )
#   }
#   
#   if (smooth) {
#     p <- p + ggplot2::geom_smooth(method = "lm", linewidth = 1.2)
#   }
#   
#   if (!is.null(facet_var)) {
#     p <- ggplot2::ggplot(df2, ggplot2::aes(x = xval, y = yval, color = facet)) +
#       ggplot2::geom_point(size = 3, alpha = 0.8)
#   } else {
#     p <- ggplot2::ggplot(df2, ggplot2::aes(x = xval, y = yval)) +
#       ggplot2::geom_point(size = 3, alpha = 0.8)
#   }
#   
#   if (flip_axes) {
#     p <- p + ggplot2::coord_flip()
#   }
#   
#   p + ggplot2::labs(
#     x = xlab %||% x,
#     y = ylab %||% y
#   )
# }

make_metric_plot <- function(df_round, df_club, metric, facet, smooth, flip, x_choice) {
  
  req(metric)
  req(facet)
  req(x_choice)
  
  if (facet == "Tee Club" &&
      !(metric %in% c("FIR % by Tee Club", "GIR % by Tee Club"))) {
    validate("Tee Club faceting is only available for tee‑club metrics.")
  }
  
  metric_map <- c(
    "FIR %"                    = "fir",
    "GIR %"                    = "gir",
    "Up & Down %"              = "updown",
    "Putts per Round"          = "tot_putts",
    "Chips per Round"          = "tot_chips",
    "Putts + Chips per Round"  = "tot_putts_and_chips",
    "Penalties per Round"      = "tot_penalties",
    "FIR % by Tee Club"        = "tee_club_fir",
    "GIR % by Tee Club"        = "tee_club_gir"
  )
  
  facet_map <- c(
    "None"     = NA_character_,
    "Course"   = "course_name",
    "Tee Club" = "tee_club"
  )
  
  metric_col <- metric_map[[metric]]
  facet_var  <- facet_map[[facet]]
  
  is_club_metric <- metric_col %in% c("tee_club_fir", "tee_club_gir")
  
  if (x_choice == "Date") {
    x_var <- "date"
    x_lab <- "Date"
    y_var <- metric_col
  } else {
    x_var <- metric_col
    x_lab <- metric
    y_var <- "tot_gross"
  }
  
  if (facet == "Tee Club") {
    df <- df_club |> dplyr::filter(!is.na(tee_club))
  } else if (is_club_metric) {
    df <- df_club |> dplyr::filter(!is.na(tee_club))
  } else {
    df <- df_round
  }
  
  # base plot
  
  if (is.na(facet_var)) {
    p <- ggplot2::ggplot(
      df,
      ggplot2::aes(
        x = .data[[x_var]],
        y = .data[[y_var]]
      )
    )
  } else {
    p <- ggplot2::ggplot(
      df,
      ggplot2::aes(
        x     = .data[[x_var]],
        y     = .data[[y_var]],
        color = .data[[facet_var]]
      )
    )
  }
  
  # geoms
  
  p <- p + ggplot2::geom_point(size = 2, alpha = 0.8) + ggplot2::theme(aspect.ratio = NULL,
                                                                       plot.margin = ggplot2::margin(5,5,5,5))
  
  if (smooth) {
    p <- p + ggplot2::geom_smooth(method = "lm", linewidth = 0.6)
  }
  
  # faceting
  
  if (!is.na(facet_var)) {
    if (facet_var == "course_name") {
      p <- p + ggplot2::facet_wrap(~course_name)
    } else if (facet_var == "tee_club") {
      if (metric_col == "tee_club_gir") {
        p <- p + ggplot2::facet_grid(par ~ tee_club)
      } else {
        p <- p + ggplot2::facet_wrap(~tee_club)
      }
    }
  }
  
  # flip?
  
  if (flip) {
    p <- p + ggplot2::coord_flip()
  }
  
  # labs
  
  if (x_choice == "Date") {
    final_x_lab <- "Date"
    final_y_lab <- metric
  } else {
    final_x_lab <- metric
    final_y_lab <- "Gross Score"
  }
  
  if (is.na(facet_var)) {
    p <- p + ggplot2::labs(
      x = final_x_lab,
      y = final_y_lab
    )
  } else {
    p <- p + ggplot2::labs(
      x = final_x_lab,
      y = final_y_lab,
      color = facet
    )
  }
  
  p
}

