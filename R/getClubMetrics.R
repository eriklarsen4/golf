#' @title getClubMetrics
#' @description
#' \strong{getClubMetrics} is a function gathers club data and harmonizes it with scores and course data
#'
#' @param course a string containing the name of the course played
#' \describe{can be one of: 
#' \itemize{
#' \item{\strong{Randolph North}}
#' \item{\strong{Dell Urich}}
#' \item{\strong{Silverbell}}
#' \item{\strong{Fred Enke}}
#' \item{\strong{Sewailo}}
#' \item{\strong{Arizona National}}
#' \item{\strong{Quarry Pines}}
#' \item{\strong{El Rio}}
#' \item{\strong{Crooked Tree}}
#' }
#' }
#'
#' @param round_date a string in YYYY-MM-DD format, specifying the date played
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
#' }} 
#'
#' @returns
#' \itemize{\strong{club_metrics}: a dataframe containing the hole, yards, club choice, type of lie, shot type and shot result for every tracked shot}
#'
#' @details
#' Prepares the data of tracked shots for data entry.
#'
#' @examples
#' df <- golf::Card |> dplyr::filter(course == 'Randolph North' & date == '2026-01-04')
#' choices_of_club <- df |> dplyr::select(club) |> unlist() |> as.character()
#' target_dist <- df |> dplyr::select(yds_to_target) |> unlist() |> as.numeric()
#' act_dist <- df |> dplyr::select(yds_traveled) |> unlist() |> as.numeric()
#' type_of_lie <- df |> dplyr::select(lie) |> unlist() |> as.character()
#' targets_YN <- df |> dplyr::select(on_target) |> unlist() |> as.character()
#' locations <- df |> dplyr::select(miss_direction) |> unlist() |> as.character()
#' shot_types <- df|> dplyr::select(shot_type) |> unlist() |> as.character()
#'  
#' getClubMetrics(
#' course = 'Randolph North',
#' round_date = '2026-01-04',
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
getClubMetrics <- function(course, round_date, club_choice, distance_to_target, distance_traveled, lie_type, target_status, location, type_of_shot){
  assertthat::assert_that(!missing(course), msg = "'course' is a required parameter! Please see help file for valid strings.")
  assertthat::assert_that(grepl(course,
                                pattern = '(Randolph|North|Randolph North)|(Dell|Urich|Dell Urich)|Silverbell|(Fred|Enke|Fred Enke)|Sewailo|(AZN|Arizona National|National)|(Quarry|Quarry Pines|QP)|El Rio|(Crooked|Crooked Tree)') |> any(),
                          msg = "Invalid 'course'! Please see help docs for proper input options.")
  assertthat::assert_that(!missing(round_date), msg = "'round_date' is a required parameter! Please see help file for valid strings.")
  assertthat::assert_that(grepl(round_date, pattern = '[0-9]{4}\\-[0-9]{2}\\-[0-9]{2}'), msg = "'round_date' requires strings in YYYY-MM-DD format!")
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
  
  con <- golf::get_db_connection(db_path = NULL)
  
  club_metrics <- data.frame('course_name' = c(rep(NA, 18)),
                             'date' = c(rep(NA, 18)),
                             'hole' = c(paste0('hole_', seq(18))),
                             'stroke_n' = c(rep(NA, 18))
                             )
  
  club_metrics1 <- club_metrics |> 
    dplyr::mutate(course_name = course, date = lubridate::as_date(date)) |> 
    dplyr::full_join(
      DBI::dbGetQuery(conn = con,
                      statement = paste0("SELECT DISTINCT r.*, c.par
                                   FROM rounds r
                                   INNER JOIN courses c
                                   ON r.course_name = c.course_name
                                   AND r.tees = c.tees
                                   AND r.hole = c.hole;")) |>
        dplyr::mutate(date = lubridate::as_date(.data$date)) |>
        dplyr::filter(grepl(date, pattern = round_date)) |> 
        dplyr::select(.data$course_name, .data$date, .data$tees, .data$hole, .data$par, .data$gross)
    ) |> 
    # add stroke records to gather how many strokes to track
    dplyr::full_join(
      DBI::dbGetQuery(conn = con,
                      statement = paste0("SELECT DISTINCT * FROM rounds;")) |>
        dplyr::mutate(date = lubridate::as_date(.data$date)) |>
        dplyr::filter(grepl(date, pattern = round_date)) |> 
        dplyr::mutate(stroke_n = DBI::dbGetQuery(conn = con,
                                                 statement = paste0("SELECT DISTINCT * FROM rounds;")) |>
                        dplyr::mutate(date = lubridate::as_date(.data$date)) |>
                        dplyr::filter(grepl(date, pattern = round_date)) |> 
                        dplyr::select(.data$gross, .data$putts, .data$chips) |> 
                        dplyr::mutate(gross = .data$gross - (.data$putts + .data$chips)) |> 
                        dplyr::select(.data$gross) |> unlist() |> as.numeric()) |> 
        dplyr::select(.data$course_name, .data$date, .data$hole, .data$stroke_n)
    ) |> 
    # re-arrange the data to allow more easy data upload (vector of manually curated variables)
    dplyr::group_by(.data$course_name, .data$date, .data$hole) |> 
    tidyr::fill(.data$tees:.data$gross, .direction = 'down') |> 
    dplyr::ungroup() |> 
    dplyr::filter(!is.na(.data$stroke_n)) |>
    dplyr::mutate(stroke = .data$stroke_n) |> 
    tidyr::uncount(.data$stroke_n, .id = '.data$stroke') |> 
    dplyr::relocate(.data$par, .after = .data$hole) |> 
    dplyr::relocate(.data$stroke, .after = .data$hole) |> 
    dplyr::mutate(gross = as.character(.data$gross), par = as.character(.data$par)) |> 
    dplyr::mutate(lie = '',
                  club = '',
                  shot_type = '',
                  yds_to_target = NA_real_,
                  yds_traveled = NA_real_,
                  on_target = '',
                  miss_direction = '')
  
  club_metrics1 <<- club_metrics1
  
  assertthat::assert_that(length(club_choice) == nrow(club_metrics1),
                          msg = "'club_choice' must have the same number of clubs as the number of tracked shots!")
  assertthat::assert_that(length(distance_to_target) == nrow(club_metrics1),
                          msg = "'distance_to_target' must have the same number of distances as the number of tracked shots!")
  assertthat::assert_that(length(distance_traveled) == nrow(club_metrics1),
                          msg = "'distance_traveled' must have the same number of distances as the number of tracked shots!")
  assertthat::assert_that(length(lie_type) == nrow(club_metrics1),
                          msg = "'lie_type' must have the same number of types of lie as the number of tracked shots!")
  assertthat::assert_that(length(target_status) == nrow(club_metrics1),
                          msg = "'target_status' must have the same number of target results as the number of tracked shots!")
  assertthat::assert_that(length(location) == nrow(club_metrics1),
                          msg = "'location' must have the same number of target locations as the number of tracked shots!")
  assertthat::assert_that(length(type_of_shot) == nrow(club_metrics1),
                          msg = "'type_of_shot' must have the same number of shot types as the number of tracked shots!")
  
  
  # upload the manually curated data from Garmin Golf
  club_metrics2 <- club_metrics1 %>%
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
    dplyr::relocate(.data$tees, .after = .data$date) |> 
    dplyr::relocate(.data$gross, .after = .data$par)
  
  return(club_metrics2)
}
