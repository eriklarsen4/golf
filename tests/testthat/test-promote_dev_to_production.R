testthat::test_that("promote_dev_to_production() is atomic and respects validation", {
  
  con <- golf::get_db_connection()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  
  # --- Setup: refresh dev tables to match production ---
  golf::refresh_dev_tables_from_production()
  
  # --- 1. Baseline: promotion should succeed silently ---
  expect_silent(golf::promote_dev_to_production())
  
  # --- 2. Inject invalid dev table (duplicate player_id) ---
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
    golf::promote_dev_to_production(),
    regexp = "Duplicate player_id"
  )
  
  # --- 3. Ensure production tables were NOT modified ---
  prod_players <- DBI::dbReadTable(con, "players")
  expect_identical(
    prod_players,
    DBI::dbReadTable(con, "players") # unchanged
  )
})
