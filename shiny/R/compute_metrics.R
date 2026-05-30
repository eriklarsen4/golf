# R/compute_metrics.R

compute_stroke_level_df <- function(rounds, courses, club_metrics) {
  
  df <- rounds |>
    dplyr::inner_join(
      courses |>
        dplyr::distinct() |>
        dplyr::select(-to_par, -slope, -course_rating, -yds, -hole_handicap)
    ) |>
    dplyr::left_join(club_metrics) |>
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
    dplyr::group_by(date) |>
    dplyr::mutate(
      fir_perc = dplyr::case_when(
        grepl(course_name, pattern = "Dell") ~ round((tot_FIR / 13) * 100, 1),
        TRUE ~ round((tot_FIR / 14) * 100, 1)
      ),
      gir_perc = round((tot_GIR / 18) * 100, 1)
    ) |>
    dplyr::ungroup()
  
  # Add per-round KPI columns expected by the module
  df <- df |>
    dplyr::mutate(
      fir             = fir_perc / 100,
      gir             = gir_perc / 100,
      updown          = scrambling_perc / 100,
      putts_round     = tot_putts,
      penalties_round = tot_penalties
    )
  
  df
}

# KPI FUNCTIONS ------

compute_kpi_fir <- function(df) {
  mean(df$fir, na.rm = TRUE)
}

compute_kpi_gir <- function(df) {
  mean(df$gir, na.rm = TRUE)
}

compute_kpi_updown <- function(df) {
  mean(df$updown, na.rm = TRUE)
}

compute_kpi_putts <- function(df) {
  mean(df$putts_round, na.rm = TRUE)
}

compute_kpi_penalties <- function(df) {
  mean(df$penalties_round, na.rm = TRUE)
}
