#' @title feature_search
#' @description
#' \strong{feature_search} is a function that searches tables throughout the database for the provided regular
#'
#' @param feature a regular expression string
#'
#' @returns
#' \itemize{\strong{result}: a dataframe containing the table and features matching the regular expression}
#'
#' @details
#' Prepares the data of tracked shots for data entry.
#'
#' @examples
#' \dontrun{
#' feature_search(feature = 'GHIN')
#' }
#'
#' @import assertthat
#' @import DBI
#' @import tibble
#' @import dplyr
#'
#' @export
# ----
feature_search <- function(feature){
  assertthat::assert_that(!missing(feature), msg = "'feature', is a required parameter!")
  assertthat::assert_that(is.character(feature), msg = "'feature' must be a regular expression (string)!")
  
  con <- golf::get_db_connection()
  
  result <- lapply(
    sapply(
      tbls, function(x) {
        DBI::dbListFields(conn = con, name = x)
      }, USE.NAMES = T
    ),
    function(y) {
      y[which(grepl(y, pattern = feature, ignore.case = F) ==  T)]
    }
  ) |> 
    t() |> 
    as.data.frame() |> 
    t() |> 
    as.data.frame() |> 
    tibble::rownames_to_column('table') |> 
    dplyr::rename('matches' = 2) |> 
    dplyr::filter(grepl(matches, pattern = feature)) |> 
    dplyr::distinct()
  
  return(result)
}