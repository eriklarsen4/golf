pkg_env <- new.env(parent = emptyenv())
pkg_env$con <- NULL

#' Create a db connection
#'
#' Lazily opens a SQLite connection and reuses it for the session
#'
#' @param db_path Path to SQLite db. If NULL, uses source-tree DB when developing.
#'
#' @return DBIConnection
#' @importFrom DBI dbConnect dbIsValid
#' @import RSQLite
#'
#' @export
get_db_connection <- function(db_path = NULL) {
  
  # Resolve default path
  if (is.null(db_path)) {
    src <- tryCatch(devtools::as.package(".")$path, error = function(e) NULL)
    if (!is.null(src)) {
      db_path <- file.path(src, "inst", "extdata", "golf_data.db")
    } else {
      installed <- system.file("extdata", "golf_data.db", package = "golf")
      stop("No db_path supplied and not running from source. Installed DB is read-only: ", installed)
    }
  }
  
  db_path <- normalizePath(db_path, mustWork = TRUE)
  
  # Reuse existing connection if valid and same file
  if (!is.null(pkg_env$con)) {
    valid <- tryCatch(DBI::dbIsValid(pkg_env$con), error = function(e) FALSE)
    if (valid) {
      current <- normalizePath(DBI::dbGetInfo(pkg_env$con)$dbname, mustWork = FALSE)
      if (identical(current, db_path)) return(pkg_env$con)
    }
    try(DBI::dbDisconnect(pkg_env$con), silent = TRUE)
  }
  
  # Open new connection
  pkg_env$con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  pkg_env$con
}