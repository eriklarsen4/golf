testthat::test_that("refresh_dev_tables() produces exact copies of production tables", {
  
  # Use a temporary writable DuckDB for testing
  test_db <- tempfile(fileext = ".duckdb")
  
  # Copy the real DB into the temp file so schema + data exist
  file.copy(
    system.file("extdata", "golf.duckdb", package = "golf"),
    test_db,
    overwrite = TRUE
  )
  
  # Connect to the temporary DB
  con <- golf::get_db_connection(db_path = test_db)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  
  # 1. Modify dev tables to ensure they differ from production
  DBI::dbExecute(con, "DELETE FROM dev_rounds")
  
  expect_false(
    identical(
      DBI::dbReadTable(con, "dev_rounds"),
      DBI::dbReadTable(con, "rounds")
    )
  )
  
  # 2. Refresh dev tables
  expect_silent(golf::refresh_dev_tables(db_path = test_db))
  
  # 3. Now they should be identical
  expect_identical(
    DBI::dbReadTable(con, "dev_rounds"),
    DBI::dbReadTable(con, "rounds")
  )
  
  expect_identical(
    DBI::dbReadTable(con, "dev_courses"),
    DBI::dbReadTable(con, "courses")
  )
  
  expect_identical(
    DBI::dbReadTable(con, "dev_players"),
    DBI::dbReadTable(con, "players")
  )
  
  expect_identical(
    DBI::dbReadTable(con, "dev_club_metrics"),
    DBI::dbReadTable(con, "club_metrics")
  )
})
