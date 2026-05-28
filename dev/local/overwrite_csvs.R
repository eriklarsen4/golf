# print(getwd())
export_dir <- "C:/Users/Erik/Desktop/Programming/R/Sports/golf/inst/extdata/golf_exports"
# print(export_dir)

overwrite_csvs <- function(db_path = NULL) {
  
  con <- golf::get_db_connection(db_path)
  
  most_recent_round_date_db <- list()
  most_recent_round_date_pbi <- list()
  
  "%notin%" <- Negate("%in%")
  
  # get tables
  listy <- DBI::dbListTables(conn = con)
  
  # initialize column names of tables list
  listy2 <- list()
  
  for (i in 1:length(listy)) {
    listy2[[i]] <- DBI::dbListFields(conn = con, name = listy[i])
  }
  names(listy2) <- listy
  
  # initialize bools list
  is_date <- list()
  
  for (i in 1:length(listy)) {
    if ('date' %in% listy2[[i]]) {
      is_date[[i]] <- TRUE
    } else {
      is_date[[i]] <- FALSE
    }
  }
  names(is_date) <- listy
  
  # find which tables need checking for overwriting power bi .csv's
  tbls_to_check <- is_date |> 
    purrr::list_flatten() |> 
    purrr::map_df(.f = as.data.frame, .id = 'table_name') |> 
    dplyr::rename(has_date = 2) |> 
    dplyr::filter(has_date == TRUE & !grepl(table_name, pattern = 'dev')) |> 
    dplyr::distinct(table_name) |> 
    dplyr::pull()
  
  
  pbi <- dir(export_dir, pattern = paste0(tbls_to_check, "\\.csv", collapse = "|"), full.names = TRUE) # all tables used by power bi
  
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
  
  for (t in seq_along(tbls_to_check)) {
    most_recent_round_date_db[[t]] <- DBI::dbGetQuery(conn = con,
                                                      statement = paste0("SELECT DISTINCT date FROM ", tbls_to_check[t], " ORDER BY date DESC LIMIT 1;")) |>
      dplyr::mutate(date = lubridate::as_date(date)) |>
      dplyr::pull(1)
    names(most_recent_round_date_db)[t] <- tbls_to_check[t]
  }
  
  # conditional overwrite
  for (t in tbls_to_check) {
    
    # skip if no matching CSV
    if (!(t %in% names(most_recent_round_date_pbi))) next
    
    if (TRUE) {
      
      # print(paste("Checking table:", t))
      # print(paste("DB date:", most_recent_round_date_db[[t]]))
      # print(paste("CSV date:", most_recent_round_date_pbi[[t]]))
      
      out_file <- file.path(export_dir, paste0(t, ".csv"))
      
      utils::write.csv(
        DBI::dbGetQuery(con, paste0("SELECT * FROM ", t)),
        file = out_file,
        row.names = FALSE
      )
    }
  }
  
}

if (identical(environment(), globalenv())) {
  overwrite_csvs()
}