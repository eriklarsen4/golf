pkg_env <- new.env(parent = emptyenv())
pkg_env$con <- NULL

#' Create a DuckDB connection
#'
#' Lazily opens a DuckDB connection and reuses it for the session
#' Uses source-tree DB during development; installed DB is read-only
#'
#' @param db_path Path to DuckDB db. If NULL, uses source-tree DB when developing.
#'
#' @return a DBIConnection
#' @import DBI
#' @import duckdb
#'
#' @export
get_db_connection <- function(db_path = NULL) {
  
  # Resolve default path
  if (is.null(db_path)) {
    src <- tryCatch(devtools::as.package(".")$path, error = function(e) NULL)
    if (!is.null(src)) {
      db_path <- file.path(src, "inst", "extdata", "golf.duckdb")
    } else {
      installed <- system.file("extdata", "golf.duckdb", package = "golf")
      stop("No db_path supplied and not running from source. Installed DB is read-only: ", installed)
    }
  }
  
  db_path <- normalizePath(db_path, mustWork = F)
  
  # Reuse existing connection if valid and same file
  if (!is.null(pkg_env$con)) {
    valid <- tryCatch(DBI::dbIsValid(pkg_env$con), error = function(e) F)
    if (valid) {
      current <- tryCatch(
        normalizePath(DBI::dbGetInfo(pkg_env$con)$dbname, mustWork = F),
        error = function(e) NA_character_
      )
      if (identical(current, db_path)) {
        return(pkg_env$con)
      }
    }
    if (DBI::dbIsValid(pkg_env$con)) {
      try(DBI::dbDisconnect(pkg_env$con, shutdown = T), silent = T)
    }
  }
  
  # Open new connection
  pkg_env$con <- DBI::dbConnect(duckdb::duckdb(), dbdir = db_path, read_only = F)
  pkg_env$con
}