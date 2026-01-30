#' @docType data
#' @keywords internal
#' @name my.env
#' @title env setup
#' @description
#' setting environment variables and connecting to the database
#' @details create a new environment for the db connection to the package's db
#' @noRd
pkg <- new.env(parent = emptyenv())
pkg$con <- NULL

#' Create a db connection
#' 
#' @param db_path the path to the db
#' @importFrom DBI dbConnect dbIsValid
#' @import RSQLite
#' 
#' @export
get_db_connection <- function(db_path = NULL) {
  
  if (is.null(db_path)) {
    db_path <- system.file('extdata', 'golf_data.db', package = 'golf')
  }
  
  if (!is.null(pkg$con)) {
    valid <- tryCatch(DBI::dbIsValid(pkg$con), error = function(e) FALSE )
    if (valid) return(pkg$con)
  }
  
  if (!is.null(pkg$con)) {
    DBI::dbDisconnect(conn = pkg$con)
  }
  
  if ( !file.exists(db_path) ) {
    stop("Database file not found: ", db_path)
  }
  
  pkg$con <- DBI::dbConnect(drv = RSQLite::SQLite(), dbname = db_path)
  pkg$con
  
}
