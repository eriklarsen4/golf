testthat::test_that("refresh_dev_tables() produces exact copies of production tables", {
  
  # Build a minimal production-like DB from backups
  test_db <- tempfile(fileext = ".duckdb")
  con <- DBI::dbConnect(duckdb::duckdb(), test_db)
  
  # Load backups as stand-ins for production tables
  prod_rounds  <- read.csv(system.file("extdata/csv_backup/rounds.csv",  package = "golf"))
  prod_courses <- read.csv(system.file("extdata/csv_backup/courses.csv", package = "golf"))
  prod_club_metrics   <- read.csv(system.file("extdata/csv_backup/club_metrics.csv",   package = "golf"))
  prod_players <- read.csv(system.file("extdata/csv_backup/club_metrics.csv", package = "golf"))
  
  DBI::dbWriteTable(con, "rounds",  prod_rounds,  overwrite = T)
  DBI::dbWriteTable(con, "courses", prod_courses, overwrite = T)
  DBI::dbWriteTable(con, "club_metrics",   prod_club_metrics,   overwrite = T)
  DBI::dbWriteTable(con, "players", prod_players, overwrite = T)
  
  # Connect to the temporary DB
  con <- golf::get_db_connection(db_path = test_db)
  on.exit(DBI::dbDisconnect(con, shutdown = T), add = T)
  
  # 1. Modify dev tables to ensure they differ from production
  DBI::dbExecute(con, "DELETE FROM dev_rounds")
  
  testthat::expect_false(
    identical(
      DBI::dbReadTable(con, "dev_rounds"),
      DBI::dbReadTable(con, "rounds")
    )
  )
  
  # 2. Refresh dev tables
  testthat::expect_silent(golf::refresh_dev_tables(db_path = test_db))
  
  # 3. Now they should be identical
  testthat::expect_identical(
    DBI::dbReadTable(con, "dev_rounds"),
    DBI::dbReadTable(con, "rounds")
  )
  
  testthat::expect_identical(
    DBI::dbReadTable(con, "dev_courses"),
    DBI::dbReadTable(con, "courses")
  )
  
  testthat::expect_identical(
    DBI::dbReadTable(con, "dev_players"),
    DBI::dbReadTable(con, "players")
  )
  
  testthat::expect_identical(
    DBI::dbReadTable(con, "dev_club_metrics"),
    DBI::dbReadTable(con, "club_metrics")
  )
})
