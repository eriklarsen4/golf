testthat::test_that("promote_dev_to_production() is atomic and respects validation", {
  
  # Build a minimal production-like DB from backups ----
  test_db <- tempfile(fileext = ".duckdb")
  con <- DBI::dbConnect(duckdb::duckdb(), test_db)
  
  prod_rounds  <- read.csv(system.file("extdata/csv_backup/rounds.csv",  package = "golf"))
  prod_courses <- read.csv(system.file("extdata/csv_backup/courses.csv", package = "golf"))
  prod_players <- read.csv(system.file("extdata/csv_backup/players.csv", package = "golf"))
  prod_club_metrics <- read.csv(system.file("extdata/csv_backup/club_metrics.csv", package = "golf"))
  
  DBI::dbWriteTable(con, "rounds",        prod_rounds,      overwrite = T)
  DBI::dbWriteTable(con, "courses",       prod_courses,     overwrite = T)
  DBI::dbWriteTable(con, "players",       prod_players,     overwrite = T)
  DBI::dbWriteTable(con, "club_metrics",  prod_club_metrics, overwrite = T)
  
  # Connect to the temporary DB
  con <- golf::get_db_connection(db_path = test_db)
  on.exit(DBI::dbDisconnect(con, shutdown = T), add = T)
  
  # Setup: refresh dev tables to match production ----
  golf::refresh_dev_tables(db_path = test_db)
  
  # 1. Baseline: promotion should succeed silently ----
  testthat::expect_silent(golf::promote_dev_to_production(db_path = test_db))
  
  # 2. Inject invalid dev table (duplicate composite key) ----
  players <- DBI::dbReadTable(con, "dev_players")
  if (nrow(players) > 0) {
    DBI::dbWriteTable(
      con, "dev_players",
      players[1, , drop = F],
      append = T,
      row.names = F
    )
  }
  
  # Promotion should fail due to validation
  testthat::expect_error(
    golf::promote_dev_to_production(db_path = test_db),
    regexp = "Duplicate players detected"
  )
  
  # 3. Ensure production tables were NOT modified ----
  prod_players_before <- DBI::dbReadTable(con, "players")
  prod_players_after  <- DBI::dbReadTable(con, "players")
  
  testthat::expect_identical(prod_players_before, prod_players_after)
})
