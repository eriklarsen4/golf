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
  
  
  pbi <- dir(file.path("inst", "extdata", "golf_exports"), pattern = paste0(tables, '\\.csv', collapse = "|"), full.names = T) # all tables used by power bi
  
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
    
    # skip unmatched .csv / db tables
    if ( !(t %in% names(most_recent_round_date_pbi)) ) {
      next
    }
    
    if ( most_recent_round_date_pbi[[t]]  < most_recent_round_date_db[[t]]
    ) {
      
      out_file <- pbi[basename(pbi) == paste0(t,".csv")]
      
      utils::write.csv(x = DBI::dbGetQuery(conn = con,
                                           statement = paste0("SELECT DISTINCT * FROM ", t, ";")),
                       file = out_file,
                       append = F,
                       row.names = F, 
                       col.names = T)
    }
  }
  
}