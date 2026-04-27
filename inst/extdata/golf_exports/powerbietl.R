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