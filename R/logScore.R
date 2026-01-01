#' @title logScore
#' @description
#' \strong{logScore} is a function that analyzes the score of a user on a specified course
#'
#' @param Scorecard a dataframe object comprising a scorecard
#' \describe{\item{\strong{Scorecard}}{: e.g. \strong{Card}}}
#' 
#' @param hole_by_hole a concatenated vector of integers comprising a player's score for each hole
#' 
#' @param GHIN (optional) a string value of integers representing a GHIN
#' 
#' @param index (optional) a float representing a player's handicap index
#' 
#' @param FIR (optional) an integer representing the number of fairways hit by a player for the given round
#' 
#' @param GIR (optional) an integer representing the number of greens hit by a player for the given round
#' 
#' @param putts (optional) an integer representing the number of putts struck by a player for the given round
#' 
#' @param chips (optional) an integer representing the number of chips struck by a player for the given round
#' 
#' @param penalties (optional) an integer representing the number of penalties incurred by a player for the given round
#' 
#' @param tee_club (optional) a concatenated vector of strings comprising a player's choice of club off the tee for every hole
#'
#' @returns
#' \item{\strong{Card}}{: a dataframe containing the record of a player's round; depending on the input, integrating the handicap to compute net strokes}
#'
#' @details
#' Completes and (pending input) analyzes the scorecard of the specific course and tees for data entry
#'
#' @examples
#' Card <- getCourse(course = 'North', date = '2025-12-01', tees = 'combo')
#' logScore(Scorecard = Card,
#'           hole_by_hole = c(5, 6, 3, 5, 5, 3, 6, 5, 3, 5, 6, 2, 5, 6, 3, 4, 4, 7),
#'           GHIN = '10526424',
#'           index = 10.4,
#'           FIR = c(1, 1, 0, 1, 1, 0, 0, 1, 0,
#'                   0, 1, 0, 1, 0, 1, 0, 0, 0),
#'           GIR = c(0, 0, 1, 0, 1, 1, 0, 1, 1,
#'                   0, 0, 1, 0, 0, 0, 0, 1, 0),
#'           putts = c(2, 2, 2, 1, 3, 2, 2, 3, 2,
#'                     2, 2, 1, 2, 2, 0, 1, 2, 3),
#'           chips = c(1, 0, 0, 1, 0, 0, 1, 0, 0,
#'                     1, 2, 0, 1, 0, 1, 1, 0, 1),
#'           penalties = c(0, 0, 0, 0, 0, 0, 0, 0, 0,
#'                         0, 0, 0, 0, 0, 0, 0, 0, 0),
#'           tee_club = c('4', '4', 'D', 'D', 'D', 'GW', '4', '7', 'D',
#'                        'D', '7', 'D', 'D', 'D', '8', 'D', 'D', 'D')
#'         )
#'
#' @import dplyr
#' @import assertthat
#'
#' @export
# ----
logScore <- function(Scorecard, hole_by_hole, GHIN = NULL, index = NULL, FIR = NULL, GIR = NULL, putts = NULL, chips = NULL, penalties = NULL, tee_club = NULL) {
  assertthat::assert_that(!missing(Scorecard), msg = "'Scorecard' is a required parameter!")
  assertthat::assert_that(!missing(hole_by_hole), msg = "'hole_by_hole' is a required parameter!")
  assertthat::assert_that(is.numeric(hole_by_hole), msg = "'hole_by_hole' must be 18 integers!")
  assertthat::assert_that(length(hole_by_hole)==18, msg = "'hole_by_hole' must be 18 integers!")
  assertthat::assert_that(is.character(GHIN), msg = "'GHIN' must be a character!")
  assertthat::assert_that(is.numeric(index), msg = "'index' must be numeric!")
  assertthat::assert_that(is.numeric(FIR), msg = "'FIR' must be 18 integers!")
  assertthat::assert_that(length(FIR)==18, msg = "'FIR' must be 18 integers!")
  assertthat::assert_that(is.numeric(GIR), msg = "'GIR' must be 18 integers!")
  assertthat::assert_that(length(GIR)==18, msg = "'GIR' must be 18 integers")
  assertthat::assert_that(is.numeric(putts), msg = "'putts' must be 18 integers!")
  assertthat::assert_that(length(putts)==18, msg = "'putts' must be 18 integers!")
  assertthat::assert_that(is.numeric(chips), msg = "'chips' must be 18 integers!")
  assertthat::assert_that(length(chips)==18, msg = "'chips' must be 18 integers!")
  assertthat::assert_that(is.numeric(penalties), msg = "'penalties' must be 18 integers!")
  assertthat::assert_that(length(penalties)==18, msg = "'penalties' must be 18 integers!")
  assertthat::assert_that(is.character(tee_club), msg = "'tee_club' must be 18 characters!")
  assertthat::assert_that(length(tee_club)==18, msg = "'tee_club' must be 18 characters!")
  if (!missing(GHIN)) {
    Scorecard <- Scorecard |> 
      dplyr::mutate(GHIN = GHIN, .before = .data$course)
  }
  if (!missing(index)) {
    Scorecard <- Scorecard |> 
      dplyr::mutate(index = index, .before = .data$course) |> 
      dplyr::mutate(handicap_stroke = dplyr::case_when(.data$hole_handicap >= round(abs(.data$to_par - (.data$course_rating + index)), 1) ~ -1,
                                                       .data$hole_handicap < round(abs(.data$to_par - (.data$course_rating + index)), 1) ~ 0)) |> 
      dplyr::mutate(gross = hole_by_hole,
                    net = hole_by_hole + .data$handicap_stroke) 
  } else {
    Scorecard <- Scorecard |> 
      dplyr::mutate(gross = hole_by_hole)
  }
  Scorecard <- Scorecard |>
    dplyr::mutate(OUT = sum(.data$gross[c(1:9)]),
                  IN = sum(.data$gross[c(10:18)])) |> 
    dplyr::mutate(is_gross_birdie = dplyr::case_when(.data$gross - .data$par == -1 ~ T, TRUE ~ F),
                  is_gross_eagle_better = dplyr::case_when(.data$gross - .data$par < -1 ~ T, TRUE ~ F),
                  is_gross_par = dplyr::case_when(.data$gross - .data$par == 0 ~ T, TRUE ~ F),
                  is_gross_bogey = dplyr::case_when(.data$gross - .data$par == 1 ~ T, TRUE ~ F),
                  is_gross_bogey_worse = dplyr::case_when(.data$gross - .data$par > 1 ~ T, TRUE ~ F))
  if (!missing(index)) {
    Scorecard <- Scorecard |> 
      dplyr::mutate(is_net_birdie = dplyr::case_when(.data$net - .data$par == -1 ~ T, TRUE ~ F),
                    is_net_eagle_better = dplyr::case_when(.data$net - .data$par < -1 ~ T, TRUE ~ F),
                    is_net_par = dplyr::case_when(.data$net - .data$par == 0 ~ T, TRUE ~ F),
                    is_net_bogey = dplyr::case_when(.data$net - .data$par == 1 ~ T, TRUE ~ F),
                    is_net_bogey_worse = dplyr::case_when(.data$net - .data$par > 1 ~ T, TRUE ~ F))
  }
  if (!missing(FIR)) {
    Scorecard <- Scorecard |> 
      dplyr::mutate(FIR = FIR,
                    OUT_FIR = sum(FIR[c(1:9)]),
                    IN_FIR = sum(FIR[c(10:18)]),
                    tot_FIR = .data$OUT_FIR + .data$IN_FIR)
  }
  if (!missing(GIR)) {
    Scorecard <- Scorecard |> 
      dplyr::mutate(GIR = GIR,
                    OUT_GIR = sum(GIR[c(1:9)]),
                    IN_GIR = sum(GIR[c(10:18)]),
                    tot_GIR = .data$OUT_GIR + .data$IN_GIR)
  }
  if (!missing(putts)) {
    Scorecard <- Scorecard |> 
      dplyr::mutate(putts = putts,
                    OUT_putts = sum(putts[c(1:9)]),
                    IN_putts = sum(putts[c(10:18)]),
                    tot_putts = .data$OUT_putts + .data$IN_putts)
  }
  if (!missing(chips)) {
    Scorecard <- Scorecard |> 
      dplyr::mutate(chips = chips,
                    OUT_chips = sum(chips[c(1:9)]),
                    IN_chips = sum(chips[c(10:18)]),
                    tot_chips = .data$OUT_chips + .data$IN_chips)
  }
  if (!missing(penalties)) {
    Scorecard <- Scorecard |> 
      dplyr::mutate(penalties = penalties,
                    OUT_penalties = sum(penalties[c(1:9)]),
                    IN_penalties = sum(penalties[c(10:18)]),
                    tot_penalties = .data$OUT_penalties + .data$IN_penalties) |> 
      dplyr::ungroup()
  }
  if (!missing(tee_club)) {
    Scorecard <- Scorecard |>
      dplyr::mutate(tee_club = tee_club)
  }
  Scorecard <- Scorecard |> 
    dplyr::mutate(tot_gross = sum(.data$gross))
  
  if (!missing(index)) {
    Scorecard <- Scorecard |> 
      dplyr::mutate(tot_net = sum(.data$net))
  }
  
  Scorecard <- Scorecard |> 
    dplyr::relocate(dplyr::contains("tot"), .after = .data$IN)
  
  Card <- Scorecard
  
  return(Card)
}