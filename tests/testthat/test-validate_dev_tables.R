testthat::test_that("validate_dev_tables() detects schema mismatches, duplicates, and impossible values", {
  
  # Build a minimal production-like DB from backups
  test_db <- tempfile(fileext = ".duckdb")
  con <- DBI::dbConnect(duckdb::duckdb(), test_db)
  
  # Load backups as stand-ins for production tables
  prod_rounds  <- read.csv(system.file("extdata/csv_backup/rounds.csv",  package = "golf"))
  prod_courses <- read.csv(system.file("extdata/csv_backup/courses.csv", package = "golf"))
  prod_club_metrics   <- read.csv(system.file("extdata/csv_backup/club_metrics.csv",   package = "golf"))
  prod_players <- read.csv(system.file("extdata/csv_backup/players.csv", package = "golf"))
  
  dev_rounds <- read.csv(system.file("extdata/csv_backup/dev_rounds.csv", package = 'golf'))
  dev_courses <- read.csv(system.file("extdata/csv_backup/dev_courses.csv", package = 'golf'))
  dev_players <- read.csv(system.file("extdata/csv_backup/dev_players.csv", package = 'golf'))
  dev_club_metrics <- read.csv(system.file("extdata/csv_backup/dev_club_metrics.csv", package = 'golf'))
  
  DBI::dbWriteTable(con, "rounds",  prod_rounds,  overwrite = T)
  DBI::dbWriteTable(con, "courses", prod_courses, overwrite = T)
  DBI::dbWriteTable(con, "club_metrics",   prod_club_metrics,   overwrite = T)
  DBI::dbWriteTable(con, "players", prod_players, overwrite = T)
  
  DBI::dbWriteTable(con, "dev_rounds",        dev_rounds,      overwrite = T)
  DBI::dbWriteTable(con, "dev_courses",       dev_courses,     overwrite = T)
  DBI::dbWriteTable(con, "dev_players",       dev_players,     overwrite = T)
  DBI::dbWriteTable(con, "dev_club_metrics",  dev_club_metrics, overwrite = T)
  
  # Ensure dev_rounds schema matches production BEFORE refresh
  DBI::dbWriteTable(con, "dev_rounds", prod_rounds[0, ], overwrite = T)
  DBI::dbDisconnect(con, shutdown = T)
  
  # Setup: refresh dev tables to ensure clean baseline
  golf::refresh_dev_tables(db_path = test_db)
  
  # (refresh_dev_tables closes db connection)
  DBI::dbDisconnect(con, shutdown = T)
  con <- golf::get_db_connection(db_path = test_db)
  
  # 1. Baseline: dev tables should validate cleanly
  testthat::expect_silent({
    res <- golf::validate_dev_tables(db_path = test_db)
  })
  testthat::expect_true(all(res))
  
  # (validate_dev_tables closes db connection)
  DBI::dbDisconnect(con, shutdown = T)
  con <- golf::get_db_connection(db_path = test_db)
  
  # 2. Inject duplicate rounds
  rounds <- DBI::dbReadTable(con, "dev_rounds")
  if (nrow(rounds) > 0) {
    DBI::dbWriteTable(
      con, "dev_rounds",
      rounds[1, , drop = F],
      append = T,
      row.names = F
    )
  }
  
  testthat::expect_error(
    golf::validate_dev_tables(db_path = test_db),
    regexp = "Duplicate rounds"
  )
  
  # (validate_dev_tables closes db connection; re-open)
  DBI::dbDisconnect(con, shutdown = T)
  con <- golf::get_db_connection(db_path = test_db)
  
  # 3. Inject impossible score
  DBI::dbExecute(con, "DELETE FROM dev_rounds")
  bad <- rounds[1, , drop = F]
  bad$tot_gross <- 999
  DBI::dbWriteTable(con, "dev_rounds", bad, append = T, row.names = F)
  
  testthat::expect_error(
    golf::validate_dev_tables(db_path = test_db),
    regexp = "impossible gross scores"
  )
  # (validate_dev_tables closes db connection; re-open)
  DBI::dbDisconnect(con, shutdown = T)
  con <- golf::get_db_connection(db_path = test_db)
  
  # 4. Inject schema mismatch
  DBI::dbExecute(con, "ALTER TABLE dev_courses ADD COLUMN bogus_col INTEGER")
  
  testthat::expect_error(
    golf::validate_dev_tables(db_path = test_db),
    regexp = "Schema mismatch"
  )
})
