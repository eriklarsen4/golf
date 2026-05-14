#' Promote dev tables to production tables
#'
#' @description
#' Copies the validated development tables into their production
#' counterparts. This function performs an atomic promotion: if any
#' part of the promotion fails, all changes are rolled back and the
#' production tables remain unchanged.
#'
#' @param db_path optional path to a DuckDB database file; if supplied, the 
#' function uses this database instead of the package's default. Designed for
#' testing with temp writable databases
#'
#' The following table mappings are promoted:
#' \itemize{
#'   \item \code{dev_rounds      -> rounds}
#'   \item \code{dev_courses     -> courses}
#'   \item \code{dev_players     -> players}
#'   \item \code{dev_club_metrics -> club_metrics}
#' }
#'
#' No schema changes are made. The production tables are cleared and
#' repopulated with the contents of the dev tables.
#'
#' @return
#' Returns \code{TRUE} invisibly if promotion succeeds.
#'
#' @import DBI
#' @export
promote_dev_to_production <- function(db_path = NULL) {
  
  con <- golf::get_db_connection(db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  
  # Validate first
  validate_dev_tables()
  
  table_pairs <- list(
    dev_rounds       = "rounds",
    dev_courses      = "courses",
    dev_players      = "players",
    dev_club_metrics = "club_metrics"
  )
  
  DBI::dbBegin(con)
  
  tryCatch({
    
    for (dev_tbl in names(table_pairs)) {
      prod_tbl <- table_pairs[[dev_tbl]]
      
      # wipe production table
      DBI::dbExecute(con, paste0("DELETE FROM ", prod_tbl))
      
      # copy dev → production
      data <- DBI::dbReadTable(con, dev_tbl)
      
      DBI::dbWriteTable(
        con,
        prod_tbl,
        data,
        append = TRUE,
        row.names = FALSE
      )
    }
    
    DBI::dbCommit(con)
    
  }, error = function(e) {
    DBI::dbRollback(con)
    stop("Promotion failed: ", e$message)
  })
  
  invisible(TRUE)
}
