#' @title env close
#' @description
#' closing database connections after detaching the package
#'
#' @import DBI
#'
#' @export
close_connection <- function(e, conn_name = "con") {
  # Want to be as defensive as possible, so if there is no connection, we don't want to test it
  if (conn_name %in% ls(e)) {
    con <- get(conn_name, envir = e)
    # If connection has closed for any other reason, we don't want the function to error
    if (DBI::dbIsValid(con)) {
      DBI::dbDisconnect(con)
    }
  }
}

.onLoad <- function(libname, pkgname) {
  reg.finalizer(
    e = .pkg,
    f = close_connection,
    onexit = TRUE
  )
}

.onUnload <- function(libpath) {
  close_connection(.pkg)
}