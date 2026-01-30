#' @title env close
#' @description
#' closing database connections after detaching the package
#'
#' @param pkg package environment variable
#' @importFrom DBI dbIsValid dbDisconnect
#'
#' @export
close_connection <- function(pkg) {
  # Want to be as defensive as possible, so if there is no connection, we don't want to test it
  for ( conn_name in ls(pkg) ) {
    con <- get(conn_name, envir = pkg)
      # If connection has closed for any other reason, we don't want the function to error
    if (!is.null(con) && DBI::dbIsValid(con)) {
      DBI::dbDisconnect(con)
    }
  }
}

#' Hook function to initialize package on load
#' 
#' @param libname path to the package library
#' @param pkgname name of the package
#' @noRd
.onLoad <- function(libname, pkgname) {
  reg.finalizer(
    e = pkg,
    f = close_connection,
    onexit = TRUE
  )
}

#' Hook function to clean up on load (close db connection)
#' @param libpath path to package
#' @noRd
.onUnload <- function(libpath) {
  close_connection(pkg)
}