#' Export round-level LM predictions to the package database
#' 
#' @title export_lm_round_predictions
#'
#' @description
#' Writes predictions from a round-level mixed-effects model into the 
#' `predictions_round_lm` table inside the package database.
#' The function appends a new time-stamped model version on each run,
#' while removing any predictions associated with the same version to
#' prevent duplication during re-knits.
#'
#' @param model a fitted model object (e.g., from \code{lmer()}).
#' @param scores_sum a data frame containing round-level features used
#'   to generate predictions. Must include \code{date} and
#'   \code{course_name}.
#'
#' @return invisibly returns the prediction data frame written to the
#'   database.
#'
#' @details
#' This function:
#' \itemize{
#'   \item connects to the package database using
#'     \code{golf::get_db_connection()},
#'   \item ensures the prediction table exists,
#'   \item generates a timestamp-based model version,
#'   \item removes any predictions from the same model version,
#'   \item computes predictions for all rows in \code{scores_sum},
#'   \item appends the new predictions to the database.
#' }
#'
#' The table schema uses \code{date} and \code{course_name} as the
#' canonical identifiers for round-level predictions.
#' 
#' @import DBI
#' @import dplyr
#' @importFrom stats predict
#' @export
export_lm_round_predictions <- function(model, scores_sum) {
  
  # timestamp-based version identifier
  model_version <- format(Sys.time(), "%Y%m%d_%H%M%S")
  generated_at  <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  
  # connect to DB
  con <- golf::get_db_connection()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  
  # ensure table exists
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS predictions_round_lm (
      player_date TEXT,
      date TEXT,
      course_name TEXT,
      predicted_score REAL,
      model_version TEXT,
      generated_at TEXT,
      features_hash TEXT,
      notes TEXT,
      PRIMARY KEY (player_date, course_name, model_version)
    );
  ")
  
  # remove predictions from this model_version (protects against re-knits)
  DBI::dbExecute(
    con,
    "DELETE FROM predictions_round_lm WHERE model_version = ?;",
    params = list(model_version)
  )
  
  # extract the required model inputs, as in the predict_score vignette, but include 'course_name' for using as table variable in the predictions_round_lm
  scores_sum <- scores_sum |>
    dplyr::ungroup() |>
    dplyr::mutate(
      # extract canonical course_name from date_course
      course_name = gsub(
        pattern = "[0-9]|\\-|\\\n|\\.",
        replacement = "", # extract the course names
        x = .data$date_course
      ),
      
      # extract the course names for model input
      course = .data$course_name,
      
      course_rating = mean(.data$course_rating) - .data$course_rating, # center the course rating
      `Handicap Index` = mean(.data$`Handicap Index`) - .data$`Handicap Index`, # center the Handicap Index
      
      days = as.numeric(as.Date(.data$date) - min(as.Date(.data$date)) + 1, units = "days") # start days from day = 1
    ) |>
    dplyr::relocate(.data$days, .after = .data$date)
  
  
  # generate predictions
  preds <- scores_sum |>
    dplyr::ungroup() |>
    dplyr::mutate(
      player_date = paste0(.data$GHIN, "_", format(as.Date(.data$date), "%Y%m%d")),
      date = format(as.Date(.data$date), "%Y%m%d"),
      predicted_score = round(as.numeric(stats::predict(model, newdata = scores_sum)), 0),
      model_version   = model_version,
      generated_at    = generated_at,
      features_hash   = NA_character_,
      notes           = NA_character_
    ) |>
    dplyr::select(
      .data$player_date,
      .data$date,
      .data$course_name,
      .data$predicted_score,
      .data$model_version,
      .data$generated_at,
      .data$features_hash,
      .data$notes
    )
  
  # write predictions
  DBI::dbWriteTable(
    con,
    "predictions_round_lm",
    preds,
    append = TRUE,
    row.names = FALSE
  )
  
  invisible(preds)
}