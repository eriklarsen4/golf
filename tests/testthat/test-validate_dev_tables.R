testthat::test_that("validate_dev_tables() detects schema mismatches, duplicates, and impossible values", {
  
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
  
  # Setup: refresh dev tables to ensure clean baseline
  golf::refresh_dev_tables(db_path = test_db)
  
  # 1. Baseline: dev tables should validate cleanly
  expect_silent({
    res <- golf::validate_dev_tables(db_path = test_db)
  })
  expect_true(all(res))
  
  # 2. Inject duplicate rounds
  rounds <- DBI::dbReadTable(con, "dev_rounds")
  if (nrow(rounds) > 0) {
    DBI::dbWriteTable(
      con, "dev_rounds",
      rounds[1, , drop = FALSE],
      append = TRUE,
      row.names = FALSE
    )
  }
  
  expect_error(
    golf::validate_dev_tables(db_path = test_db),
    regexp = "Duplicate rounds"
  )
  
  # 3. Inject impossible score
  DBI::dbExecute(con, "DELETE FROM dev_rounds")
  bad <- rounds[1, , drop = FALSE]
  bad$gross_score <- 999
  DBI::dbWriteTable(con, "dev_rounds", bad, append = TRUE, row.names = FALSE)
  
  expect_error(
    golf::validate_dev_tables(),
    regexp = "impossible gross scores"
  )
  
  # 4. Inject schema mismatch
  DBI::dbExecute(con, "ALTER TABLE dev_courses ADD COLUMN bogus_col INTEGER")
  
  expect_error(
    golf::validate_dev_tables(),
    regexp = "Schema mismatch"
  )
})
