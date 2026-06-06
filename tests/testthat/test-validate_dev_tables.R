testthat::test_that("validate_dev_tables() detects schema mismatches, duplicates, and impossible values", {
  
  # Build a minimal production-like DB from backups
  test_db <- tempfile(fileext = ".duckdb")
  con <- DBI::dbConnect(duckdb::duckdb(), test_db)
  
  # Load backups as stand-ins for production tables
  prod_rounds  <- read.csv(system.file("extdata/csv_backup/rounds.csv",  package = "golf"))
  prod_courses <- read.csv(system.file("extdata/csv_backup/courses.csv", package = "golf"))
  prod_club_metrics   <- read.csv(system.file("extdata/csv_backup/club_metrics.csv",   package = "golf"))
  prod_players <- read.csv(system.file("extdata/csv_backup/players.csv", package = "golf"))
  
  DBI::dbWriteTable(con, "rounds",  prod_rounds,  overwrite = T)
  DBI::dbWriteTable(con, "courses", prod_courses, overwrite = T)
  DBI::dbWriteTable(con, "club_metrics",   prod_club_metrics,   overwrite = T)
  DBI::dbWriteTable(con, "players", prod_players, overwrite = T)
  
  # Setup: refresh dev tables to ensure clean baseline
  golf::refresh_dev_tables(db_path = test_db)
  
  # 1. Baseline: dev tables should validate cleanly
  testthat::expect_silent({
    res <- golf::validate_dev_tables(db_path = test_db)
  })
  testthat::expect_true(all(res))
  
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
  
  # 3. Inject impossible score
  DBI::dbExecute(con, "DELETE FROM dev_rounds")
  bad <- rounds[1, , drop = F]
  bad$gross_score <- 999
  DBI::dbWriteTable(con, "dev_rounds", bad, append = T, row.names = F)
  
  testthat::expect_error(
    golf::validate_dev_tables(),
    regexp = "impossible gross scores"
  )
  
  # 4. Inject schema mismatch
  DBI::dbExecute(con, "ALTER TABLE dev_courses ADD COLUMN bogus_col INTEGER")
  
  testthat::expect_error(
    golf::validate_dev_tables(),
    regexp = "Schema mismatch"
  )
})
