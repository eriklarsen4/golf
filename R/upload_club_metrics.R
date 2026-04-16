#' @title upload_club_metrics
#' @description
#' \strong{upload_club_metrics} is a function that harmonizes the Garmin shot tracking data
#' with round scores data
#'
#' @param club_metrics a data frame object containing the results returned from \link[golf]{get_tracked_shots_data_shape}
#' 
#' @param club_choice a character vector specifying the club selection for a tracked stroke
#' \describe{can be any one of: 
#' \itemize{
#' \item{\strong{D}: Driver}
#' \item{\strong{3W}: 3-Wood}
#' \item{\strong{4}: 4-Iron}
#' \item{\strong{5}: 5-Iron}
#' \item{\strong{6}: 6-Iron}
#' \item{\strong{7}: 7-Iron}
#' \item{\strong{8}: 8-Iron}
#' \item{\strong{9}: 9-Iron}
#' \item{\strong{PW}: Pitching Wedge}
#' \item{\strong{GW}: Gap Wedge}
#' \item{\strong{SW}: Sand Wedge}
#' \item{\strong{LW}: Lob Wedge}
#' \item{\strong{P}: Putter}
#' }
#' }
#' 
#' @param distance_to_target a numeric vector specifying the distance to the target of each tracked shot
#' 
#' @param distance_traveled a numeric vector specifying the distance the ball traveled for each tracked shot
#' 
#' @param lie_type a character vector specifying the type of lie of each tracked shot
#' \describe{values can be one of:
#' \itemize{
#' \item{\strong{tee}: ball off the tee}
#' \item{\strong{fairway}:  ball from the fairway}
#' \item{\strong{rough}: ball from the rough}
#' \item{\strong{fwbunker}: ball from a fairway bunker}
#' \item{\strong{bsbunker}: ball fro a greenside bunker}
#' }
#' }
#' 
#' @param target_status a character vector of yes/no values specifying whether the tracked shot hit its target
#' 
#' @param location a character vector specifying where the ball resulted after the tracked shot
#' \describe{can be one of: 
#' \itemize{
#' \item{\strong{left}}
#' \item{\strong{right}}
#' \item{\strong{short}}
#' \item{\strong{long}}
#' \item{\strong{on_target}}
#' }
#' }
#' 
#' @param type_of_shot a character vector specifying the type of swing for the tracked shot
#' \describe{cane be one of: 
#' \itemize{
#' \item{\strong{full}}
#' \item{\strong{choked}}
#' \item{\strong{punch}}
#' \item{\strong{chip}}
#' \item{\strong{fwbunker}}
#' \item{\strong{gsbunker}}
#' \item{\strong{chip}}
#' \item{\strong{putt}}
#' }} 
#'
#' @returns
#' \itemize{\strong{club_metrics}: a dataframe containing the hole, yards, club choice, type of lie, shot type and shot result for every tracked shot}
#'
#' @details
#' Prepares the data of tracked shots for data entry.
#'
#' @examples
#' # first, get the correct tracked shots for a given round
#' club_metrics_df <- get_tracked_shots_data_shape(round_date = '2026-02-08')
#' df <- golf::Card |> dplyr::filter(course == 'Randolph North' & date == '2026-02-08')
#' choices_of_club <- df |> dplyr::select(club) |> unlist() |> as.character()
#' target_dist <- df |> dplyr::select(yds_to_target) |> unlist() |> as.numeric()
#' act_dist <- df |> dplyr::select(yds_traveled) |> unlist() |> as.numeric()
#' type_of_lie <- df |> dplyr::select(lie) |> unlist() |> as.character()
#' targets_YN <- df |> dplyr::select(on_target) |> unlist() |> as.character()
#' locations <- df |> dplyr::select(miss_direction) |> unlist() |> as.character()
#' shot_types <- df|> dplyr::select(shot_type) |> unlist() |> as.character()
#'  
#' upload_club_metrics(
#' club_metrics = club_metrics_df,
#' club_choice = choices_of_club,
#' distance_to_target = target_dist,
#' distance_traveled = act_dist,
#' lie_type = type_of_lie,
#' target_status = targets_YN,
#' location = locations,
#' type_of_shot = shot_types)
#'
#' @import assertthat
#' @import DBI
#' @import RSQLite
#' @import lubridate
#' @import dplyr
#' @import tidyr
#'
#' @export
# ----
upload_club_metrics <- function(club_metrics, club_choice, distance_to_target, distance_traveled, lie_type, target_status, location, type_of_shot){
  assertthat::assert_that(!missing(club_metrics), msg = "'club_metrics', is a required parameter! Please use 'club_metrics_df' as required input!")
  assertthat::assert_that(is.data.frame(club_metrics), msg = "'club_metrics', is a required parameter! Please make sure the passed object is a data frame!")
  assertthat::assert_that(!missing(club_choice), msg = "'club_choice' is a required parameter! Please see help file for valid strings.")
  assertthat::assert_that(is.character(club_choice), msg = "'club_choice' must be a character vector! Please see help file for valid strings.")
  assertthat::assert_that(!missing(distance_to_target), msg = "'distance_to_target' is a required parameter!")
  assertthat::assert_that(is.numeric(distance_to_target), msg = "'distance_to_target' must be a numeric vector!")
  assertthat::assert_that(!missing(distance_traveled), msg = "'distance_traveled' is a required parameter!")
  assertthat::assert_that(is.numeric(distance_traveled), msg = "'distance_traveled' must be a numeric vector!")
  assertthat::assert_that(!missing(lie_type), msg = "'lie_type' is a required parameter! Please see help file for valid strings.")
  assertthat::assert_that(is.character(lie_type), msg = "'lie_type' must be a character vector! Please see help file for valid strings.")
  assertthat::assert_that(!missing(target_status), msg = "'target_status' is a required parameter! Please see help file for valid strings.")
  assertthat::assert_that(is.character(target_status), msg = "'target_status' must be a character vector! Please see help file for valid strings.")
  assertthat::assert_that(!missing(location), msg = "'location' is a required parameter! Please see help file for valid strings.")
  assertthat::assert_that(is.character(location), msg = "'location' must be a character vector! Please see help file for valid strings.")
  assertthat::assert_that(!missing(type_of_shot), msg = "'type_of_shot' is a required parameter! Please see help file for valid strings.")
  assertthat::assert_that(is.character(type_of_shot), msg = "'type_of_shot' must be a character vector! Please see help file for valid strings.")
  
  # con <- golf::get_db_connection(db_path = NULL)
  
  club_metrics1 <- club_metrics |> 
    # re-arrange the data to allow more easy data upload (vector of manually curated variables)
    dplyr::group_by(.data$course_name, .data$date, .data$hole) |> 
    tidyr::fill(.data$tees:.data$gross, .direction = 'down') |> 
    dplyr::ungroup() |> 
    dplyr::filter(!is.na(.data$tracked_shots)) |>
    tidyr::uncount(.data$tracked_shots, .id = 'stroke') |> 
    dplyr::mutate(gross = as.character(.data$gross), par = as.character(.data$par)) |> 
    dplyr::mutate(lie = '',
                  club = '',
                  shot_type = '',
                  yds_to_target = NA_real_,
                  yds_traveled = NA_real_,
                  on_target = '',
                  miss_direction = '')
  
  # upload the manually curated data from Garmin Golf
  club_metrics2 <- club_metrics1 |>
    dplyr::mutate(dplyr::across(c(.data$par, dplyr::contains('yds'), .data$gross), ~as.numeric(.x))) |> 
    dplyr::mutate(club = club_choice,
                  lie = lie_type,
                  shot_type = type_of_shot,
                  yds_to_target = distance_to_target,
                  yds_traveled = distance_traveled,
                  on_target = target_status,
                  miss_direction = location) |> 
    dplyr::mutate(par = as.integer(.data$par), gross = as.integer(.data$gross)) |> 
    dplyr::filter(!is.na(.data$club)) |> 
    dplyr::select(-c(dplyr::contains("tracked"))) |> 
    dplyr::distinct()
  
  return(club_metrics2)
}
