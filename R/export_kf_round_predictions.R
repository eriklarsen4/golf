#' Export round-level KF predictions to the package database
#'
#' @title export_kf_round_predictions
#'
#' @description
#' Writes predictions and skill-model outputs from the lmer + Kalman filter
#' pipeline into the `predictions_round` table inside the package database.
#' A timestamp-based model version is generated on each run, and any existing
#' predictions associated with the same version are removed to prevent
#' duplication during re-knits.
#'
#' @param predictions_round A data frame containing round-level predictions
#'   and derived quantities from the lmer + Kalman filter pipeline. Must
#'   include at least:
#'   \code{date}, \code{course_name}, \code{Gross Score},
#'   \code{expected_gross}, \code{expected_rel_par},
#'   \code{skill_est}, \code{skill_ci_lower}, \code{skill_ci_upper},
#'   \code{skill_adj_gross}, \code{skill_adj_rel_par},
#'   \code{sg_self}, \code{index_true}, \code{index_posted},
#'   and \code{index_gap}.
#'
#' @return Invisibly returns the prediction data frame written to the
#'   database.
#'
#' @details
#' This function:
#' \itemize{
#'   \item connects to the package database using
#'     \code{golf::get_db_connection()},
#'   \item ensures the \code{predictions_round} table exists,
#'   \item generates a timestamp-based model version,
#'   \item removes any predictions from the same model version,
#'   \item appends the new predictions to the database.
#' }
#'
#' The table schema stores all skill-model quantities needed for downstream
#' analysis and Power BI reporting.
#'
#' @import DBI
#' @import dplyr
#' @export
export_kf_round_predictions <- function(predictions_round) {
  
  # timestamp-based version identifier
  model_version <- format(Sys.time(), "%Y%m%d_%H%M%S")
  generated_at  <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  
  # connect to DB
  con <- golf::get_db_connection()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  
  # ensure table exists
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS predictions_round (
      date TEXT,
      course_name TEXT,
      tees TEXT,
      course_rating REAL,
      course_par REAL,
      gross_score REAL,
      expected_gross REAL,
      expected_rel_par REAL,
      residual REAL,
      skill_est REAL,
      skill_ci_lower REAL,
      skill_ci_upper REAL,
      skill_adj_gross REAL,
      skill_adj_rel_par REAL,
      skill_rel_par REAL,
      sg_self REAL,
      index_posted REAL,
      index_true REAL,
      index_gap REAL,
      model_version TEXT,
      generated_at TEXT,
      PRIMARY KEY (date, course_name, model_version)
    );
  ")
  
  # remove predictions from this model_version (protects against re-knits)
  DBI::dbExecute(
    con,
    "DELETE FROM predictions_round WHERE model_version = ?;",
    params = list(model_version)
  )
  
  # prepare rows for writing
  preds <- predictions_round |>
    dplyr::ungroup() |>
    dplyr::mutate(
      date          = format(as.Date(.data$date), "%Y%m%d"),
      model_version = model_version,
      generated_at  = generated_at
    )
  
  # write predictions
  DBI::dbWriteTable(
    con,
    "predictions_round",
    preds,
    append = TRUE,
    row.names = FALSE
  )
  
  invisible(preds)
}
