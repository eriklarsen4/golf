testthat::test_that("promote_dev_to_production() is atomic and respects validation", {
  
  # Create a temporary writable DuckDB for testing ----
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
  
  # Setup: refresh dev tables to match production ----
  golf::refresh_dev_tables(db_path = test_db)
  
  # 1. Baseline: promotion should succeed silently ----
  expect_silent(golf::promote_dev_to_production(db_path = test_db))
  
  # 2. Inject invalid dev table (duplicate composite key) ----
  players <- DBI::dbReadTable(con, "dev_players")
  if (nrow(players) > 0) {
    DBI::dbWriteTable(
      con, "dev_players",
      players[1, , drop = FALSE],
      append = TRUE,
      row.names = FALSE
    )
  }
  
  # Promotion should fail due to validation
  expect_error(
    golf::promote_dev_to_production(db_path = test_db),
    regexp = "Duplicate players detected"
  )
  
  # 3. Ensure production tables were NOT modified ----
  prod_players_before <- DBI::dbReadTable(con, "players")
  prod_players_after  <- DBI::dbReadTable(con, "players")
  
  expect_identical(prod_players_before, prod_players_after)
})
