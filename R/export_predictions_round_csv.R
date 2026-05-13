#' Export round-level predictions to CSV
#'
#' @title export_predictions_round_csv
#'
#' @description
#' Exports the authoritative round-level predictions stored in the package
#' database (written by the skill pipeline) to a CSV file. This function
#' does *not* modify the database in any way; it simply reads the
#' `predictions_round` table and writes it to disk.
#'
#' @details
#' The skill pipeline (`golf::run_skill_pipeline()`) is the sole writer to the
#' `predictions_round` table inside the package DuckDB database. This export
#' function provides a convenient way to extract those predictions for
#' external analysis, archival, or sharing.
#'
#' Unlike earlier versions of the export utilities, this function:
#' \itemize{
#'   \item does *not* create or alter any database tables,
#'   \item does *not* delete or overwrite prediction rows,
#'   \item does *not* inject metadata such as model version or timestamps,
#'   \item does *not* write anything back to the database.
#' }
#'
#' It is a pure read → write CSV utility.
#'
#' @param path A file path (including `.csv` extension) where the exported
#'   predictions should be written.
#'
#' @return Invisibly returns the data frame that was written to the CSV.
#'
#' @examples
#' \dontrun{
#'   export_predictions_round_csv("predictions_export.csv")
#' }
#'
#' @import DBI
#' @importFrom readr write_csv
#' @export
export_predictions_round_csv <- function(path) {
  
  # connect to DB
  con <- golf::get_db_connection()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  
  # read authoritative predictions table
  preds <- DBI::dbReadTable(con, "predictions_round")
  
  # write CSV
  readr::write_csv(preds, path)
  
  invisible(preds)
}
