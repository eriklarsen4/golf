#' Close the package-managed database connection
#' 
#' Safely disconnects the SQLite connection stored in pkg_env
#'
#' @param e Environment containing the connection (default: pkg_env)
#' @param conn_name Name of the connection object inside the environment
#' @importFrom DBI dbIsValid dbDisconnect
#' @import DBI
#'
#' @export
close_connection <- function(e = pkg_env, conn_name = "con") {
  
  if (conn_name %in% ls(e)) {
    con <- e[[conn_name]]
    
    if (!is.null(con) && DBI::dbIsValid(con)) {
      DBI::dbDisconnect(con)
    }
  }
}
