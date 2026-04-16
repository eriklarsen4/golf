# Internal package environment for DB connection
pkg <- new.env(parent = emptyenv())
pkg$con <- NULL

#' Create a db connection
#' 
#' Lazily opens a SQLite connection and reuses it for the session
#' 
#' @param db_path the path to the SQLite db. If NULL, uses the package's extdata/golf_data.db file
#' 
#' @return a DBIConnection object
#' @importFrom DBI dbConnect dbIsValid
#' @import RSQLite
#' 
#' @export
get_db_connection <- function(db_path = NULL) {
  
  # Default DB path inside the installed package
  if (is.null(db_path)) {
    db_path <- system.file("extdata", "golf_data.db", package = "golf")
  }
  
  # Reuse existing valid connection
  if (!is.null(pkg_env$con)) {
    valid <- tryCatch(DBI::dbIsValid(pkg_env$con), error = function(e) FALSE)
    if (valid) return(pkg_env$con)
  }
  
  # Close stale connection
  if (!is.null(pkg_env$con)) {
    try(DBI::dbDisconnect(pkg_env$con), silent = TRUE)
  }
  
  # Check file exists
  if (!file.exists(db_path)) {
    stop("Database file not found: ", db_path)
  }
  
  # Open new connection
  pkg_env$con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  pkg_env$con
  
}
