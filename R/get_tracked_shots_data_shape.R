#' @title get_tracked_shots_data_shape
#' @description
#' \strong{get_tracked_shots_data_shape} is a function that confirms the data shape of Garmin-tracked shots (all official non-putts).
#'
#' @param round_date a string in YYYY-MM-DD format, specifying the date played.
#'
#' @returns
#' \itemize{\strong{club_metrics_df}: a dataframe containing the course_name, date, tees, par, gross hole score, and number of tracked shots}
#'
#' @details
#' Prepares the data of tracked shots for data entry.
#'
#' @examples
#' \dontrun{
#' get_tracked_shots_data_shape(round_date = '2026-02-08')
#' }
#' 
#'
#' @import assertthat
#' @import DBI
#' @import dplyr
#'
#' @export
# ----
get_tracked_shots_data_shape <- function(round_date){
  assertthat::assert_that(!missing(round_date), msg = "'round_date' is a required parameter! Please see help file for valid strings.")
  assertthat::assert_that(grepl(round_date, pattern = '[0-9]{4}\\-[0-9]{2}\\-[0-9]{2}'), msg = "'round_date' requires strings in YYYY-MM-DD format!")
  
  con <- golf::get_db_connection() 
  
  club_metrics_df <- DBI::dbGetQuery(conn = con, statement = paste0("SELECT DISTINCT * FROM dev_rounds;")) |>
    dplyr::mutate(date = as.character(.data$date)) |>
    dplyr::filter(grepl(.data$date, pattern = round_date)) |> 
    dplyr::mutate(tracked_shots = .data$gross - .data$putts - .data$penalties ) |> 
    dplyr::full_join(
      DBI::dbGetQuery(conn = con, statement = paste0("SELECT DISTINCT r.*, c.par FROM dev_rounds r
                                                   INNER JOIN dev_courses c
                                                   ON r.course_name = c.course_name
                                                   AND r.tees = c.tees
                                                   AND r.hole = c.hole;")) |> 
        dplyr::mutate(date = as.character(.data$date)) |> 
        dplyr::filter(grepl(.data$date, pattern = round_date))
    ) |> 
    dplyr::select(.data$course_name, .data$date, .data$tees, .data$hole, .data$par, .data$gross, .data$tracked_shots)
  
  return(club_metrics_df)
}