testthat::test_that("refresh_dev_tables() produces exact copies of production tables", {
  
  con <- golf::get_db_connection()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  
  # --- 1. Modify dev tables to ensure they differ from production ---
  DBI::dbExecute(con, "DELETE FROM dev_rounds")
  
  # dev_rounds should now differ from rounds
  expect_false(
    identical(
      DBI::dbReadTable(con, "dev_rounds"),
      DBI::dbReadTable(con, "rounds")
    )
  )
  
  # --- 2. Refresh dev tables ---
  expect_silent(golf::refresh_dev_tables())
  
  # --- 3. Now they should be identical ---
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
