#' @title getCourse
#' @description
#' \strong{getCourse} is a function that specifies the course played by a user
#'
#' @param course a string comprising one of:
#'    "Randolph North"
#'    "Dell Urich"
#'    "Silverbell"
#'    "Fred Enke"
#'    "Sewailo"
#'    "Arizona National"
#'    "Quarry Pines"
#'    "El Rio"
#'    "Crooked Tree"
#'
#' @param date a string in YYYY-MM-DD format, specifying the date played
#' 
#' @param tees a string specifying the tees from which the round was played. one of:
#' \describe{
#' \itemize{
#'    \item{\strong{white}}
#'    \item{\strong{combo}: long blue/white combination}
#'    \item{\strong{blue}}
#' }
#' }
#'
#' @returns
#' \itemize{\strong{Card}: a dataframe containing the holes, yards, and hole handicap of the specified course and tees}
#'
#' @details
#' Prepares the scorecard of the specific course and tees for data entry.
#'
#' @examples
#' getCourse(course = 'North', date = '2025-12-01', tees = 'combo')
#'
#' @import lubridate
#' @import dplyr
#' @import tidyr
#' @import assertthat
#'
#' @export
# ----
getCourse <- function(course, date, tees){
  assertthat::assert_that(!missing(course), msg = "'course' is a required parameter! Please see help file for valid strings.")
  assertthat::assert_that(grepl(course, pattern = '(Randolph|North|Randolph North)|(Dell|Urich|Dell Urich)|Silverbell|(Fred|Enke|Fred Enke)|Sewailo|(AZN|Arizona National|National)|(Quarry|Quarry Pines|QP)') |> any(),
                          msg = "Invalid 'course'! Please see help docs for proper input options.")
  assertthat::assert_that(!missing(date), msg = "'date' is a required parameter! Please see help file for valid strings.")
  assertthat::assert_that(grepl(date, pattern = '[0-9]{4}\\-[0-9]{2}\\-[0-9]{2}'), msg = "'date' requires strings in YYYY-MM-DD format!")
  assertthat::assert_that(!missing(tees), msg = "'tees' is a required parameter! Please see help file for vlaid strings.")
  assertthat::assert_that(grepl(tees, pattern = 'blue|white|(long|combo|long combo|blue white combo)'), 
                          msg = "Invalid 'tees'! Please see help docs for proper input options.")
  if (grepl(course, pattern = 'Randolph|North|Randolph North') |> any() ) {
    Scorecard <- data.frame(course = 'Randolph North',
                            date = lubridate::as_date(date),
                            tees = tees,
                            to_par = 72,
                            slope = NA,
                            course_rating = NA)
    if ( grepl(tees, pattern = 'blue') |> any() ) {
      Scorecard <- Scorecard |> 
        dplyr::mutate(
          hole_1 = 365,
          hole_2 = 415,
          hole_3 = 569,
          hole_4 = 429,
          hole_5 = 440,
          hole_6 = 136,
          hole_7 = 373,
          hole_8 = 198,
          hole_9 = 469,
          hole_10 = 423,
          hole_11 = 197,
          hole_12 = 388,
          hole_13 = 480,
          hole_14 = 432,
          hole_15 = 193,
          hole_16 = 574,
          hole_17 = 368,
          hole_18 = 434
        ) |> 
        tidyr::pivot_longer(cols = c(tidyr::contains("hole_")), names_to = 'hole', values_to = 'yds')
    } else if ( grepl(tees, pattern = 'white') |> any() ) {
      Scorecard <- Scorecard |> 
        dplyr::mutate(
          hole_1 = 354,
          hole_2 = 337,
          hole_3 = 550,
          hole_4 = 412,
          hole_5 = 411,
          hole_6 = 126,
          hole_7 = 342,
          hole_8 = 181,
          hole_9 = 441,
          hole_10 = 411,
          hole_11 = 187,
          hole_12 = 369,
          hole_13 = 471,
          hole_14 = 405,
          hole_15 = 172,
          hole_16 = 560,
          hole_17 = 360,
          hole_18 = 407
        ) |> 
        tidyr::pivot_longer(cols = c(tidyr::contains("hole_")), names_to = 'hole', values_to = 'yds')
    } else {
      Scorecard <- Scorecard |> 
        dplyr::mutate(
          hole_1 = 365,
          hole_2 = 337,
          hole_3 = 550,
          hole_4 = 412,
          hole_5 = 411,
          hole_6 = 136,
          hole_7 = 373,
          hole_8 = 181,
          hole_9 = 469,
          hole_10 = 411,
          hole_11 = 187,
          hole_12 = 388,
          hole_13 = 480,
          hole_14 = 432,
          hole_15 = 172,
          hole_16 = 560,
          hole_17 = 368,
          hole_18 = 407
        ) |> 
        tidyr::pivot_longer(cols = c(tidyr::contains("hole_")), names_to = 'hole', values_to = 'yds')
    }
    Scorecard <- Scorecard |> 
      dplyr::mutate(
        par = c(4, 4, 5, 4, 4, 3, 4, 3, 5,
                4, 3, 4, 5, 4, 3, 5, 4, 4),
        hole_handicap = c(7, 9, 11, 1, 3, 13, 15, 5, 17,
                          2, 14, 10, 16, 8, 6, 18, 12, 4),
        tees = tees,
        slope = dplyr::case_when(grepl(tees, pattern = 'blue') ~ 126,
                                 grepl(tees, pattern = 'white') ~ 120,
                                 !grepl(tees, pattern = 'white|blue') ~ 123),
        course_rating = dplyr::case_when(grepl(tees, pattern = 'blue') ~ 71.7,
                                         grepl(tees, pattern = 'white') ~ 69.8,
                                         !grepl(tees, pattern = 'white|blue') ~ 70.4)
      )
  } else if ( grepl(course, pattern = 'Dell|Dell Urich|Urich') |> any() ) {
    Scorecard <- data.frame(course = 'Dell Urich',
                            date = lubridate::as_date(date),
                            tees = tees,
                            to_par = 70,
                            slope = NA,
                            course_rating = NA)
    if ( grepl(tees, pattern = 'blue') |> any() ) {
      Scorecard <- Scorecard |> 
        dplyr::mutate(
          hole_1 = 373,
          hole_2 = 213,
          hole_3 = 557,
          hole_4 = 299,
          hole_5 = 156,
          hole_6 = 540,
          hole_7 = 445,
          hole_8 = 457,
          hole_9 = 187,
          hole_10 = 390,
          hole_11 = 193,
          hole_12 = 451,
          hole_13 = 515,
          hole_14 = 468,
          hole_15 = 429,
          hole_16 = 380,
          hole_17 = 137,
          hole_18 = 439
        ) |> 
        tidyr::pivot_longer(cols = c(tidyr::contains("hole_")), names_to = 'hole', values_to = 'yds')
    } else if ( grepl(tees, pattern = 'white') |> any() ) {
      Scorecard <- Scorecard |> 
        dplyr::mutate(
          hole_1 = 347,
          hole_2 = 170,
          hole_3 = 534,
          hole_4 = 277,
          hole_5 = 143,
          hole_6 = 527,
          hole_7 = 402,
          hole_8 = 437,
          hole_9 = 170,
          hole_10 = 364,
          hole_11 = 173,
          hole_12 = 411,
          hole_13 = 502,
          hole_14 = 427,
          hole_15 = 360,
          hole_16 = 350,
          hole_17 = 126,
          hole_18 = 401
        ) |> 
        tidyr::pivot_longer(cols = c(tidyr::contains("hole_")), names_to = 'hole', values_to = 'yds')
    } else {
      Scorecard <- Scorecard |> 
        dplyr::mutate(
          hole_1 = 373,
          hole_2 = 170,
          hole_3 = 557,
          hole_4 = 299,
          hole_5 = 156,
          hole_6 = 540,
          hole_7 = 402,
          hole_8 = 437,
          hole_9 = 170,
          hole_10 = 390,
          hole_11 = 173,
          hole_12 = 411,
          hole_13 = 515,
          hole_14 = 427,
          hole_15 = 360,
          hole_16 = 380,
          hole_17 = 137,
          hole_18 = 439
        )
    }
    Scorecard <- Scorecard |> 
      dplyr::mutate(
        par = c(4, 3, 5, 4, 3, 5, 4, 4, 3,
                4, 3, 4, 5, 4, 4, 4, 3, 4),
        hole_handicap = c(9, 5, 13, 17, 11, 15, 1, 3, 7,
                          10, 16, 6, 18, 4, 14, 8, 12, 2),
        tees = tees,
        slope = dplyr::case_when(grepl(tees, pattern = 'blue') ~ 124,
                                 grepl(tees, pattern = 'white') ~ 116,
                                 !grepl(tees, pattern = 'white|blue') ~ 120),
        course_rating = dplyr::case_when(grepl(tees, pattern = 'blue') ~ 70.3,
                                         grepl(tees, pattern = 'white') ~ 67.8,
                                         !grepl(tees, pattern = 'white|blue') ~ 68.5)
      )
  } else if ( grepl(course, pattern = 'Silverbell') |> any() ) {
    Scorecard <- data.frame(course = 'Silverbell',
                            date = lubridate::as_date(date),
                            tees = tees,
                            to_par = 72,
                            slope = NA,
                            course_rating = NA)
    if ( grepl(tees, pattern = 'blue') |> any() ) {
      Scorecard <- Scorecard |> 
        dplyr::mutate(
          hole_1 = 470,
          hole_2 = 559,
          hole_3 = 152,
          hole_4 = 497,
          hole_5 = 370,
          hole_6 = 220,
          hole_7 = 595,
          hole_8 = 450,
          hole_9 = 195,
          hole_10 = 383,
          hole_11 = 395,
          hole_12 = 200,
          hole_13 = 440,
          hole_14 = 465,
          hole_15 = 343,
          hole_16 = 237,
          hole_17 = 410,
          hole_18 = 555
        ) |> 
        tidyr::pivot_longer(cols = c(tidyr::contains("hole_")), names_to = 'hole', values_to = 'yds')
    } else if ( grepl(tees, pattern = 'white') |> any() ) {
      Scorecard <- Scorecard |> 
        dplyr::mutate(
          hole_1 = 440,
          hole_2 = 522,
          hole_3 = 143,
          hole_4 = 455,
          hole_5 = 336,
          hole_6 = 186,
          hole_7 = 548,
          hole_8 = 395,
          hole_9 = 177,
          hole_10 = 335,
          hole_11 = 378,
          hole_12 = 178,
          hole_13 = 420,
          hole_14 = 430,
          hole_15 = 316,
          hole_16 = 214,
          hole_17 = 380,
          hole_18 = 538
        ) |> 
        tidyr::pivot_longer(cols = c(tidyr::contains("hole_")), names_to = 'hole', values_to = 'yds')
    } else {
      Scorecard <- Scorecard |> 
        dplyr::mutate(
          hole_1 = 440,
          hole_2 = 559,
          hole_3 = 152,
          hole_4 = 455,
          hole_5 = 370,
          hole_6 = 186,
          hole_7 = 595,
          hole_8 = 395,
          hole_9 = 177,
          hole_10 = 383,
          hole_11 = 395,
          hole_12 = 178,
          hole_13 = 420,
          hole_14 = 430,
          hole_15 = 343,
          hole_16 = 214,
          hole_17 = 410,
          hole_18 = 555
        )|> 
        tidyr::pivot_longer(cols = c(tidyr::contains("hole_")), names_to = 'hole', values_to = 'yds')
    }
    Scorecard <- Scorecard |> 
      dplyr::mutate(
        par = c(4, 5, 3, 4, 4, 3, 5, 4, 3,
                4, 4, 3, 4, 4, 4, 3, 4, 5),
        hole_handicap = c(5, 3, 17, 9, 13, 11, 1, 7, 15,
                          14, 10, 18, 8, 4, 16, 12, 6, 2),
        tees = tees,
        slope = dplyr::case_when(grepl(tees, pattern = 'blue') ~ 124,
                                 grepl(tees, pattern = 'white') ~ 121,
                                 !grepl(tees, pattern = 'white|blue') ~ 120),
        course_rating = dplyr::case_when(grepl(tees, pattern = 'blue') ~ 71.6,
                                         grepl(tees, pattern = 'white') ~ 68.9,
                                         !grepl(tees, pattern = 'white|blue') ~ 70.3)
      )
  } else if ( grepl(course, pattern = 'Fred|Enke|Fred Enke') |> any() ) {
    Scorecard <- data.frame(course = 'Fred Enke',
                            date = lubridate::as_date(date),
                            tees = tees,
                            to_par = 72,
                            slope = NA,
                            course_rating = NA)
    if ( grepl(tees, pattern = 'blue') |> any() ) {
      Scorecard <- Scorecard |> 
        dplyr::mutate(
          hole_1 = 401,
          hole_2 = 547,
          hole_3 = 211,
          hole_4 = 321,
          hole_5 = 402,
          hole_6 = 151,
          hole_7 = 345,
          hole_8 = 393,
          hole_9 = 495,
          hole_10 = 368,
          hole_11 = 223,
          hole_12 = 507,
          hole_13 = 340,
          hole_14 = 553,
          hole_15 = 157,
          hole_16 = 388,
          hole_17 = 375,
          hole_18 = 452
        ) |> 
        tidyr::pivot_longer(cols = c(tidyr::contains("hole_")), names_to = 'hole', values_to = 'yds')
    } else if ( grepl(tees, pattern = 'white') |> any() ) {
      Scorecard <- Scorecard |> 
        dplyr::mutate(
          hole_1 = 379,
          hole_2 = 508,
          hole_3 = 185,
          hole_4 = 300,
          hole_5 = 342,
          hole_6 = 130,
          hole_7 = 333,
          hole_8 = 349,
          hole_9 = 466,
          hole_10 = 338,
          hole_11 = 176,
          hole_12 = 466,
          hole_13 = 318,
          hole_14 = 533,
          hole_15 = 122,
          hole_16 = 369,
          hole_17 = 328,
          hole_18 = 412
        )|> 
        tidyr::pivot_longer(cols = c(tidyr::contains("hole_")), names_to = 'hole', values_to = 'yds')
    } else {
      Scorecard <- Scorecard |> 
        dplyr::mutate(
          hole_1 = 368,
          hole_2 = 176,
          hole_3 = 466,
          hole_4 = 340,
          hole_5 = 553,
          hole_6 = 122,
          hole_7 = 388,
          hole_8 = 375,
          hole_9 = 412,
          hole_10 = 401,
          hole_11 = 508,
          hole_12 = 211,
          hole_13 = 300,
          hole_14 = 342,
          hole_15 = 151,
          hole_16 = 333,
          hole_17 = 393,
          hole_18 = 466
        )|> 
        tidyr::pivot_longer(cols = c(tidyr::contains("hole_")), names_to = 'hole', values_to = 'yds')
    }
    Scorecard <- Scorecard |> 
      dplyr::mutate(
        par = c(4, 5, 3, 4, 4, 3, 4, 4, 5,
                4, 3, 5, 4, 5, 3, 4, 4, 4),
        hole_handicap = c(12, 6, 8, 16, 4, 18, 14, 2, 10,
                          13, 9, 5, 15, 3, 17, 11, 7, 1),
        tees = tees,
        slope = dplyr::case_when(grepl(tees, pattern = 'blue') ~ 130,
                                 grepl(tees, pattern = 'white') ~ 120,
                                 !grepl(tees, pattern = 'white|blue') ~ 124),
        course_rating = dplyr::case_when(grepl(tees, pattern = 'blue') ~ 71.4,
                                         grepl(tees, pattern = 'white') ~ 68.6,
                                         !grepl(tees, pattern = 'white|blue') ~ 70.0)
      )
  } else if ( grepl(course, pattern = 'El Rio') |> any() ) {
    Scorecard <- data.frame(course = 'El Rio',
                            date = lubridate::as_date(date),
                            tees = tees,
                            to_par = 70,
                            slope = NA,
                            course_rating = NA)
    if ( grepl(tees, pattern = 'blue') |> any() ) {
      Scorecard <- Scorecard |> 
        dplyr::mutate(
          hole_1 = 394,
          hole_2 = 406,
          hole_3 = 427,
          hole_4 = 117,
          hole_5 = 442,
          hole_6 = 189,
          hole_7 = 376,
          hole_8 = 343,
          hole_9 = 511,
          hole_10 = 353,
          hole_11 = 446,
          hole_12 = 344,
          hole_13 = 203,
          hole_14 = 322,
          hole_15 = 366,
          hole_16 = 417,
          hole_17 = 219,
          hole_18 = 561
        )|> 
        tidyr::pivot_longer(cols = c(tidyr::contains("hole_")), names_to = 'hole', values_to = 'yds')
    } else if ( grepl(tees, pattern = 'white') |> any() ) {
      Scorecard <- Scorecard |> 
        dplyr::mutate(
          hole_1 = 368,
          hole_2 = 373,
          hole_3 = 400,
          hole_4 = 103,
          hole_5 = 418,
          hole_6 = 158,
          hole_7 = 356,
          hole_8 = 320,
          hole_9 = 492,
          hole_10 = 323,
          hole_11 = 425,
          hole_12 = 322,
          hole_13 = 176,
          hole_14 = 306,
          hole_15 = 356,
          hole_16 = 388,
          hole_17 = 193,
          hole_18 = 534
        )|> 
        tidyr::pivot_longer(cols = c(tidyr::contains("hole_")), names_to = 'hole', values_to = 'yds')
    } else {
      Scorecard <- Scorecard |> 
        dplyr::mutate(
          hole_1 = 394,
          hole_2 = 373,
          hole_3 = 400,
          hole_4 = 117,
          hole_5 = 418,
          hole_6 = 158,
          hole_7 = 376,
          hole_8 = 343,
          hole_9 = 492,
          hole_10 = 323,
          hole_11 = 425,
          hole_12 = 344,
          hole_13 = 176,
          hole_14 = 322,
          hole_15 = 366,
          hole_16 = 388,
          hole_17 = 193,
          hole_18 = 534
        )|> 
        tidyr::pivot_longer(cols = c(tidyr::contains("hole_")), names_to = 'hole', values_to = 'yds')
    }
    Scorecard <- Scorecard |> 
      dplyr::mutate(
        par = c(4, 4, 4, 3, 4, 3, 4, 4, 5,
                4, 4, 4, 3, 4, 4, 4, 3, 5),
        hole_handicap = c(12, 6, 4, 18, 2, 16, 8, 14, 10,
                          13, 1, 17, 7, 11, 15, 5, 3, 9),
        tees = tees,
        slope = dplyr::case_when(grepl(tees, pattern = 'blue') ~ 124,
                                 grepl(tees, pattern = 'white') ~ 119,
                                 !grepl(tees, pattern = 'white|blue') ~ 121),
        course_rating = dplyr::case_when(grepl(tees, pattern = 'blue') ~ 69.4,
                                         grepl(tees, pattern = 'white') ~ 67.4,
                                         !grepl(tees, pattern = 'blue|white') ~ 68.2)
      )
  } else if ( grepl(course, pattern = 'Sewailo') |> any() ) {
    Scorecard <- data.frame(course = 'Sewailo',
                            date = lubridate::as_date(date),
                            tees = tees,
                            to_par = 72,
                            slope = NA,
                            course_rating = NA)
    if ( grepl(tees, pattern = 'blue') |> any() ) {
      Scorecard <- Scorecard |> 
        dplyr::mutate(
          hole_1 = 332,
          hole_2 = 334,
          hole_3 = 138,
          hole_4 = 384,
          hole_5 = 445,
          hole_6 = 527,
          hole_7 = 180,
          hole_8 = 547,
          hole_9 = 390,
          hole_10 = 618,
          hole_11 = 456,
          hole_12 = 404,
          hole_13 = 203,
          hole_14 = 555,
          hole_15 = 153,
          hole_16 = 310,
          hole_17 = 421,
          hole_18 = 390
        )|> 
        tidyr::pivot_longer(cols = c(tidyr::contains("hole_")), names_to = 'hole', values_to = 'yds')
    } else if ( grepl(tees, pattern = 'white') |> any() ) {
      Scorecard <- Scorecard |> 
        dplyr::mutate(
          hole_1 = 328,
          hole_2 = 311,
          hole_3 = 131,
          hole_4 = 345,
          hole_5 = 431,
          hole_6 = 488,
          hole_7 = 157,
          hole_8 = 544,
          hole_9 = 354,
          hole_10 = 581,
          hole_11 = 428,
          hole_12 = 376,
          hole_13 = 176,
          hole_14 = 528,
          hole_15 = 134,
          hole_16 = 289,
          hole_17 = 383,
          hole_18 = 306
        )|> 
        tidyr::pivot_longer(cols = c(tidyr::contains("hole_")), names_to = 'hole', values_to = 'yds')
    } else {
      Scorecard <- Scorecard |> 
        dplyr::mutate(
          hole_1 = 328,
          hole_2 = 334,
          hole_3 = 138,
          hole_4 = 345,
          hole_5 = 445,
          hole_6 = 488,
          hole_7 = 157,
          hole_8 = 547,
          hole_9 = 390,
          hole_10 = 581,
          hole_11 = 428,
          hole_12 = 376,
          hole_13 = 176,
          hole_14 = 555,
          hole_15 = 153,
          hole_16 = 289,
          hole_17 = 421,
          hole_18 = 306
        )|> 
        tidyr::pivot_longer(cols = c(tidyr::contains("hole_")), names_to = 'hole', values_to = 'yds')
    }
    Scorecard <- Scorecard |> 
      dplyr::mutate(
        par = c(4, 4, 3, 4, 4, 5, 3, 5, 4,
                5, 4, 4, 3, 5, 3, 4, 4, 4),
        hole_handicap = c(8, 16, 10, 2, 4, 18, 12, 14, 6,
                          1, 3, 9, 13, 7, 17, 15, 5, 11),
        tees = tees,
        slope = dplyr::case_when(grepl(tees, pattern = 'blue') ~ 132,
                                 grepl(tees, pattern = 'white') ~ 124,
                                 !grepl(tees, pattern = 'white|blue|combo') ~ 126),
        course_rating = dplyr::case_when(grepl(tees, pattern = 'blue') ~ 71.0,
                                         grepl(tees, pattern = 'white') ~ 68.9,
                                         !grepl(tees, pattern = 'blue|white|combo') ~ 69.9)
      )
  } else if ( grepl(course, pattern = 'AZN|Arizona National|National') |> any() ){
    Scorecard <- data.frame(course = 'Arizona National',
                            date = lubridate::as_date(date),
                            tees = tees,
                            to_par = 71,
                            slope = NA,
                            course_rating = NA)
    if ( grepl(tees, pattern = 'blue') |> any() ) {
      Scorecard <- Scorecard |> 
        dplyr::mutate(
          hole_1 = 344,
          hole_2 = 518,
          hole_3 = 302,
          hole_4 = 168,
          hole_5 = 489,
          hole_6 = 138,
          hole_7 = 359,
          hole_8 = 292,
          hole_9 = 192,
          hole_10 = 412,
          hole_11 = 547,
          hole_12 = 169,
          hole_13 = 435,
          hole_14 = 387,
          hole_15 = 376,
          hole_16 = 335,
          hole_17 = 172,
          hole_18 = 497
        )|> 
        tidyr::pivot_longer(cols = c(tidyr::contains("hole_")), names_to = 'hole', values_to = 'yds')
    } else if ( grepl(tees, pattern = 'white') |> any() ) {
      Scorecard <- Scorecard |> 
        dplyr::mutate(
          hole_1 = 319,
          hole_2 = 508,
          hole_3 = 272,
          hole_4 = 137,
          hole_5 = 468,
          hole_6 = 129,
          hole_7 = 328,
          hole_8 = 266,
          hole_9 = 165,
          hole_10 = 372,
          hole_11 = 477,
          hole_12 = 161,
          hole_13 = 377,
          hole_14 = 343,
          hole_15 = 346,
          hole_16 = 273,
          hole_17 = 131,
          hole_18 = 433
        )|> 
        tidyr::pivot_longer(cols = c(tidyr::contains("hole_")), names_to = 'hole', values_to = 'yds')
    } else {
      Scorecard <- Scorecard |> 
        dplyr::mutate(
          hole_1 = 344,
          hole_2 = 518,
          hole_3 = 302,
          hole_4 = 168,
          hole_5 = 489,
          hole_6 = 138,
          hole_7 = 359,
          hole_8 = 292,
          hole_9 = 192,
          hole_10 = 372,
          hole_11 = 477,
          hole_12 = 161,
          hole_13 = 377,
          hole_14 = 343,
          hole_15 = 346,
          hole_16 = 273,
          hole_17 = 131,
          hole_18 = 433
        )|> 
        tidyr::pivot_longer(cols = c(tidyr::contains("hole_")), names_to = 'hole', values_to = 'yds')
    }
    Scorecard <- Scorecard |> 
      dplyr::mutate(
        par = c(4, 5, 4, 3, 5, 3, 4, 4, 3,
                4, 5, 3, 4, 4, 4, 4, 3, 5),
        hole_handicap = c(10, 2, 18, 8, 4, 16, 12, 14, 6,
                          3, 1, 17, 9, 7, 5, 13, 11, 15),
        tees = tees,
        slope = dplyr::case_when(grepl(tees, pattern = 'blue') ~ 132,
                                 grepl(tees, pattern = 'white') ~ 124,
                                 !grepl(tees, pattern = 'white|blue') ~ 127),
        course_rating = dplyr::case_when(grepl(tees, pattern = 'blue') ~ 68.9,
                                         grepl(tees, pattern = 'white') ~ 65.8,
                                         !grepl(tees, pattern = 'blue|white') ~ 67.3)
      )
  } else if ( grepl(course, pattern = 'Quarry|Quarry Pines|QP') |> any() ){
    Scorecard <- data.frame(course = 'Quarry Pines',
                            date = lubridate::as_date(date),
                            tees = tees,
                            to_par = 71,
                            slope = NA,
                            course_rating = NA)
    if ( grepl(tees, pattern = 'blue') |> any() ) {
      Scorecard <- Scorecard |> 
        dplyr::mutate(
          hole_1 = 524,
          hole_2 = 332,
          hole_3 = 417,
          hole_4 = 320,
          hole_5 = 490,
          hole_6 = 200,
          hole_7 = 501,
          hole_8 = 202,
          hole_9 = 302,
          hole_10 = 334,
          hole_11 = 256,
          hole_12 = 574,
          hole_13 = 368,
          hole_14 = 405,
          hole_15 = 134,
          hole_16 = 561,
          hole_17 = 197,
          hole_18 = 365
        )|> 
        tidyr::pivot_longer(cols = c(tidyr::contains("hole_")), names_to = 'hole', values_to = 'yds')
    } else if ( grepl(tees, pattern = 'white') |> any() ) {
      Scorecard <- Scorecard |> 
        dplyr::mutate(
          hole_1 = 477,
          hole_2 = 276,
          hole_3 = 359,
          hole_4 = 272,
          hole_5 = 355,
          hole_6 = 165,
          hole_7 = 452,
          hole_8 = 146,
          hole_9 = 249,
          hole_10 = 289,
          hole_11 = 171,
          hole_12 = 521,
          hole_13 = 299,
          hole_14 = 299,
          hole_15 = 106,
          hole_16 = 497,
          hole_17 = 134,
          hole_18 = 304
        )|> 
        tidyr::pivot_longer(cols = c(tidyr::contains("hole_")), names_to = 'hole', values_to = 'yds')
    } else {
      Scorecard <- Scorecard |> 
        dplyr::mutate(
          hole_1 = 485,
          hole_2 = 296,
          hole_3 = 388,
          hole_4 = 296,
          hole_5 = 411,
          hole_6 = 172,
          hole_7 = 467,
          hole_8 = 176,
          hole_9 = 276,
          hole_10 = 308,
          hole_11 = 183,
          hole_12 = 546,
          hole_13 = 340,
          hole_14 = 334,
          hole_15 = 115,
          hole_16 = 529,
          hole_17 = 177,
          hole_18 = 350
        )|> 
        tidyr::pivot_longer(cols = c(tidyr::contains("hole_")), names_to = 'hole', values_to = 'yds')
    }
    Scorecard <- Scorecard |> 
      dplyr::mutate(
        par = c(5, 4, 4, 4, 4, 3, 5, 3, 4,
                4, 3, 5, 4, 4, 3, 5, 3, 4),
        hole_handicap = c(5, 11, 3, 9, 7, 15, 1, 13, 17,
                          8, 10, 2, 16, 6, 18, 14, 12),
        tees = tees,
        slope = dplyr::case_when(grepl(tees, pattern = 'blue') ~ 135,
                                 grepl(tees, pattern = 'white') ~ 117,
                                 !grepl(tees, pattern = 'white|blue|combo') ~ 127),
        course_rating = dplyr::case_when(grepl(tees, pattern = 'blue') ~ 71.1,
                                         grepl(tees, pattern = 'white') ~ 66.7,
                                         !grepl(tees, pattern = 'blue|white|combo') ~ 68.8)
      )
  } else if ( grepl(course, pattern = 'Crooked')) {
    Scorecard <- data.frame(course = 'Crooked Tree',
                            date = lubridate::as_date(date),
                            tees = tees,
                            to_par = 72,
                            slope = NA,
                            course_rating = NA)
    if ( grepl(tees, pattern = 'blue') |> any() ) {
      Scorecard <- Scorecard |> 
        dplyr::mutate(
          hole_1 = 370,
          hole_2 = 170,
          hole_3 = 373,
          hole_4 = 575,
          hole_5 = 375,
          hole_6 = 485,
          hole_7 = 134,
          hole_8 = 550,
          hole_9 = 396,
          hole_10 = 381,
          hole_11 = 527,
          hole_12 = 360,
          hole_13 = 175,
          hole_14 = 429,
          hole_15 = 590,
          hole_16 = 252,
          hole_17 = 375,
          hole_18 = 443
        )|> 
        tidyr::pivot_longer(cols = c(tidyr::contains("hole_")), names_to = 'hole', values_to = 'yds')
    } else if ( grepl(tees, pattern = 'white') |> any() ) {
      Scorecard <- Scorecard |> 
        dplyr::mutate(
          hole_1 = 336,
          hole_2 = 153,
          hole_3 = 349,
          hole_4 = 490,
          hole_5 = 347,
          hole_6 = 364,
          hole_7 = 130,
          hole_8 = 512,
          hole_9 = 353,
          hole_10 = 363,
          hole_11 = 482,
          hole_12 = 341,
          hole_13 = 147,
          hole_14 = 397,
          hole_15 = 521,
          hole_16 = 191,
          hole_17 = 356,
          hole_18 = 352
        )|> 
        tidyr::pivot_longer(cols = c(tidyr::contains("hole_")), names_to = 'hole', values_to = 'yds')
    } else {
      Scorecard <- Scorecard |> 
        dplyr::mutate(
          hole_1 = 370,
          hole_2 = 170,
          hole_3 = 373,
          hole_4 = 575,
          hole_5 = 375,
          hole_6 = 364,
          hole_7 = 134,
          hole_8 = 512,
          hole_9 = 396,
          hole_10 = 381,
          hole_11 = 527,
          hole_12 = 360,
          hole_13 = 175,
          hole_14 = 397,
          hole_15 = 521,
          hole_16 = 252,
          hole_17 = 375,
          hole_18 = 352
        )|> 
        tidyr::pivot_longer(cols = c(tidyr::contains("hole_")), names_to = 'hole', values_to = 'yds')
    }
    Scorecard <- Scorecard |> 
      dplyr::mutate(
        par = c(4, 3, 4, 5, 4, 4, 3, 5, 4,
                4, 5, 4, 3, 4, 5, 3, 4, 4),
        hole_handicap = c(4, 18, 10, 8, 14, 12, 16, 2, 6,
                          9, 7, 11, 15, 5, 1, 17, 3, 13),
        tees = tees,
        slope = dplyr::case_when(grepl(tees, pattern = 'blue') ~ 133,
                                 grepl(tees, pattern = 'white') ~ 123,
                                 !grepl(tees, pattern = 'white|blue') ~ 129),
        course_rating = dplyr::case_when(grepl(tees, pattern = 'blue') ~ 72.5,
                                         grepl(tees, pattern = 'white') ~ 68.6,
                                         !grepl(tees, pattern = 'blue|white') ~ 70.6)
      )
  }
  Card <- Scorecard
  return(Card)
}