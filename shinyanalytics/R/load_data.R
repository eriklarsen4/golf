# R/load_data.R

load_data <- function(data_dir = "inst/extdata/golf_exports") {
  
  rounds <- utils::read.csv(file.path(data_dir, "rounds.csv")) |>
    # dplyr::mutate(hole = gsub(hole, pattern = 'hole_', replacement = '') |> as.numeric()) |> 
    dplyr::mutate(
      date = lubridate::parse_date_time(
        date,
        orders = c("Ymd", "Y-m-d", "m/d/Y", "d-m-Y"),
        exact = F
      ) |> as.Date(),
      date_js = as.numeric(date) * 86400000,
      course = course_name
    )
  
  courses <- utils::read.csv(file.path(data_dir, "courses.csv")) #|> 
    # dplyr::mutate(hole = gsub(hole, pattern = 'hole_', replacement = '') |> as.numeric())
  
  club_metrics <- utils::read.csv(file.path(data_dir, "club_metrics.csv")) |>
    # dplyr::mutate(hole = gsub(hole, pattern = 'hole_', replacement = '') |> as.numeric()) |> 
    dplyr::mutate(
      date = lubridate::parse_date_time(
        date,
        orders = c("Ymd", "Y-m-d", "m/d/Y", "d-m-Y"),
        exact = F
      ) |> as.Date()
    )
  
  skill_df <- utils::read.csv(file.path(data_dir, "predictions_round.csv")) |> 
    dplyr::mutate(
      date = lubridate::parse_date_time(
        date,
        orders = c("Ymd", "Y-m-d", "m/d/Y", "d-m-Y"),
        exact = F
      ) |> as.Date()
    )
  
  list(
    rounds = rounds,
    courses = courses,
    club_metrics = club_metrics,
    skill_df = skill_df
  )
}
