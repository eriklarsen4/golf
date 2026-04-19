#' @title update power bi .csv's
#' 
#' @description function used to check the db of round dates written to tables and overwrite .csv's if not up-to-date
#'
#' This regenerates every .csv file in inst/extdata/golf_exports
#' @param most_recent_round_date a YYYY-MM-DD formatted string variable of the most recent round
#' 
#' @examples
#' \dontrun{
#' golf::overwrite_csvs(most_recent_round_date = '2026-04-05')
#' }
#' 
#' @import DBI
#' @import readr
#' @import dplyr
#' @import stringr
#' @import utils
#' @export
overwrite_csvs <- function(most_recent_round_date) {
  
  con <- golf::get_db_connection()
  
  most_recent_round_date_db <- list()
  most_recent_round_date_pbi <- list()
  
  "%notin%" <- Negate("%in%")
  
  tables <- c(DBI::dbListTables(conn = con))[which(DBI::dbListTables(conn = con) %notin% 'courses' == T)] # db tables with date as column
  pbi <- dir(file.path("inst", "extdata", "golf_exports"), pattern = paste0(tables, '.csv', collapse = "|"), full.names = T) # all tables used by power bi
  
  most_recent_round_date_pbi <- lapply(pbi, function(x){
    readr::read_csv(x, col_names = T, show_col_types = F) |> 
      as.data.frame() |> 
      dplyr::mutate(date = lubridate::as_date(date)) |> # if previously date
      dplyr::distinct(date) |> 
      dplyr::arrange(desc(date)) |> 
      dplyr::first() |> 
      dplyr::pull()
  })
  
  names(most_recent_round_date_pbi) <- stringr::str_extract(pbi, pattern = "[^/]+(?=\\.csv$)")
  
  for (t in seq_along(tables)) {
    most_recent_round_date_db[[t]] <- DBI::dbGetQuery(conn = con,
                                                      statement = paste0("SELECT DISTINCT date FROM ", tables[t], " ORDER BY date DESC LIMIT 1;")) |>
      dplyr::mutate(date = lubridate::as_date(date)) |>
      dplyr::pull(1)
    names(most_recent_round_date_db)[t] <- tables[t]
  }
  
  for (t in tables) {
      utils::write.csv(x = DBI::dbGetQuery(conn = con,
                                    statement = paste0("SELECT DISTINCT * FROM ", t, ";")),
                file = pbi[which(!is.na(stringr::str_extract(pbi, pattern = paste0("(", t,")$"))))], append = F, row.names = F, col.names = T)
  }
  
}