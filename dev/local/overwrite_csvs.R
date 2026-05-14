overwrite_csvs <- function(db_path = NULL) {
  
  con <- golf::get_db_connection(db_path)
  
  most_recent_round_date_db <- list()
  most_recent_round_date_pbi <- list()
  
  "%notin%" <- Negate("%in%")
  
  tables <- c(DBI::dbListTables(conn = con))[which(DBI::dbListTables(conn = con) %notin% 'courses' == T)] # db tables with date as column
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
  
  for (t in seq_along(tables)) {
    most_recent_round_date_db[[t]] <- DBI::dbGetQuery(conn = con,
                                                      statement = paste0("SELECT DISTINCT date FROM ", tables[t], " ORDER BY date DESC LIMIT 1;")) |>
      dplyr::mutate(date = lubridate::as_date(date)) |>
      dplyr::pull(1)
    names(most_recent_round_date_db)[t] <- tables[t]
  }
  
  # conditional overwrite
  for (t in tables) {
    
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