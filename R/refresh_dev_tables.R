#' Refresh dev tables from production tables
#'
#' @description
#' Replaces the development (staging) tables with fresh copies of the
#' corresponding production tables. This ensures that the dev tables
#' begin in a clean, up-to-date state before new scorecard data is
#' appended in \code{scorecard_update.Rmd}.
#'
#' This function performs a simple table-level copy:
#' \itemize{
#'   \item \code{rounds          -> dev_rounds}
#'   \item \code{courses         -> dev_courses}
#'   \item \code{players         -> dev_players}
#'   \item \code{club_metrics    -> dev_club_metrics}
#' }
#'
#' No schema changes are made. The dev tables are cleared and repopulated
#' with the exact contents of the production tables. All column names and
#' types must already match between production and dev tables.
#'
#' @details
#' This function is intended to be called at the beginning of the
#' scorecard ingestion workflow. After refreshing the dev tables,
#' new rounds, courses, players, and club metrics are appended to the
#' dev tables, validated, and then promoted to production.
#'
#' @return
#' Invisibly returns \code{TRUE} on success.
#'
#' @import DBI
#' @export
refresh_dev_tables <- function() {
  
  con <- golf::get_db_connection()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  
  table_pairs <- list(
    rounds       = "dev_rounds",
    courses      = "dev_courses",
    players      = "dev_players",
    club_metrics = "dev_club_metrics"
  )
  
  DBI::dbBegin(con)
  
  tryCatch({
    
    for (prod_tbl in names(table_pairs)) {
      dev_tbl <- table_pairs[[prod_tbl]]
      
      # wipe dev table
      DBI::dbExecute(con, paste0("DELETE FROM ", dev_tbl))
      
      # copy production → dev
      data <- DBI::dbReadTable(con, prod_tbl)
      
      DBI::dbWriteTable(
        con,
        dev_tbl,
        data,
        append = TRUE,
        row.names = FALSE
      )
    }
    
    DBI::dbCommit(con)
    
  }, error = function(e) {
    DBI::dbRollback(con)
    stop("Dev table refresh failed: ", e$message)
  })
  
  invisible(TRUE)
}
