# Package lifecycle hooks

.onLoad <- function(libname, pkgname) {
  # Register a finalizer so the DB connection closes when R exits
  reg.finalizer(pkg_env, close_connection, onexit = TRUE)
}

.onUnload <- function(libpath) {
  # Explicitly close the connection when the package unloads
  close_connection(pkg_env)
}