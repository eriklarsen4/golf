# R/compute_metrics.R

compute_stroke_level_df <- function(rounds, courses, club_metrics) {
  
  # combines round (hole by hole) data
    # with 'club_metrics' data (stroke by stroke)
    # adds hole information from 'courses' first
  
  df <- rounds |>
    dplyr::inner_join(
      courses |>
        dplyr::distinct() |>
        dplyr::select(-to_par, -slope, -course_rating, -yds, -hole_handicap) |> 
        dplyr::distinct()
    ) |>
    dplyr::mutate(date = as.Date(date)) |> 
    dplyr::left_join(club_metrics |> dplyr::mutate(date = as.Date(date))) |>
    dplyr::mutate(
      scrambling_opps = dplyr::case_when(GIR == 0 ~ 1, TRUE ~ 0),
      scrambles       = dplyr::case_when(GIR == 0 & is_gross_par == 1 ~ 1, TRUE ~ 0)
    ) |>
    dplyr::group_by(date) |>
    dplyr::mutate(
      scramble_perc = round((sum(scrambles) / sum(scrambling_opps)) * 100, 1)
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      scrambling_perc = round(mean(scramble_perc, na.rm = TRUE), 1)
    ) |>
    dplyr::group_by(date, hole) |>
    dplyr::mutate(tot_putts_and_chips = tot_putts + tot_chips, .after = tot_chips) |> 
    dplyr::group_by(date) |> 
    dplyr::mutate(
      fir_perc = dplyr::case_when(
        grepl(course_name, pattern = "Dell|Quarry|Rio") ~ round((tot_FIR / 13) * 100, 1),
        T ~ round((tot_FIR / 14) * 100, 1)
      ),
      gir_perc = round((tot_GIR / 18) * 100, 1)
    ) |>
    dplyr::ungroup() |>
    dplyr::rename(fir = fir_perc) |> 
    dplyr::rename(gir = gir_perc) |> 
    dplyr::rename(updown = scramble_perc) |> 
    dplyr::mutate(FIR_opps = dplyr::if_else(par > 3, 1, NA_real_),
                  GIR_opps = 1,
                  tee_club_fir = dplyr::case_when(FIR_opps == 1 & FIR == 1 ~ 1,
                                                  is.na(FIR_opps) ~ NA_real_,
                                                  FIR_opps == 1 & FIR != 1 ~ 0,
                                                  FIR_opps == 1 & FIR == 1 ~ 1,
                                                  TRUE ~ NA_real_),
                  tee_club_gir = dplyr::case_when(GIR_opps == 1 & GIR == 1 ~ 1,
                                                  TRUE ~ 0))
  
  df
}

compute_stroke_quality <- function(stroke_level_df) {
  stroke_level_df |>
    dplyr::mutate(
      dplyr::across(dplyr::contains("yds"), ~as.numeric(.x)),
      yd_diff   = yds_to_target - yds_traveled,
      on_target = dplyr::case_when(on_target == "yes" ~ 1, TRUE ~ 0)
    )
}

compute_full_stroke_quality_avg <- function(stroke_quality) {
  stroke_quality |>
    dplyr::filter(grepl(shot_type, pattern = "full|tee")) |>
    dplyr::group_by(club) |>
    dplyr::summarize(
      avg_yds_to_target = round(mean(yds_to_target,  na.rm = T), 1),
      avg_yds_traveled  = round(mean(yds_traveled,   na.rm = T), 1),
      min_yds_traveled = round(min(yds_traveled, na.rm = T), 1),
      max_yds_traveled = round(max(yds_traveled, na.rm = T), 1),
      sd_yds_traveled   = round(sd(yds_traveled,     na.rm = T), 1),
      avg_yd_diff       = round(mean(yd_diff,        na.rm = T), 1),
      avg_accuracy      = round((sum(on_target,      na.rm = T) / dplyr::n()) * 100, 2),
      sd_accuracy       = round((sd(on_target,       na.rm = T) / dplyr::n()) * 100, 2),
      n                 = dplyr::n(),
      .groups = "drop"
    ) |>
    dplyr::rename(`club strokes` = n)
}

compute_all_metrics <- function(raw) {
  
  stroke_level_df <- compute_stroke_level_df(
    rounds       = raw$rounds,
    courses      = raw$courses,
    club_metrics = raw$club_metrics
  )
  
  stroke_quality <- compute_stroke_quality(stroke_level_df)
  
  full_stroke_quality_avg <- compute_full_stroke_quality_avg(stroke_quality)
  
  list(
    stroke_level_df         = stroke_level_df,
    stroke_quality          = stroke_quality,
    full_stroke_quality_avg = full_stroke_quality_avg
  )
}

# KPI FUNCTIONS ------

compute_kpi_fir <- function(df) {
  mean(df$fir, na.rm = T)
}

compute_kpi_gir <- function(df) {
  mean(df$gir, na.rm = T)
}

compute_kpi_updown <- function(df) {
  mean(df$scrambling_perc, na.rm = T)
}

compute_kpi_putts <- function(df) {
  mean(df$total_putts, na.rm = T)
}

compute_kpi_penalties <- function(df) {
  mean(df$total_penalties, na.rm = T)
}
