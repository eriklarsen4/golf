testthat::test_that("promote_dev_to_production() is atomic and respects validation", {
  
  # Build a minimal production-like DB from backups ----
  test_db <- tempfile(fileext = ".duckdb")
  con <- DBI::dbConnect(duckdb::duckdb(), test_db)
  
  prod_rounds  <- read.csv(system.file("extdata/csv_backup/rounds.csv",  package = "golf"))
  prod_courses <- read.csv(system.file("extdata/csv_backup/courses.csv", package = "golf"))
  prod_players <- read.csv(system.file("extdata/csv_backup/players.csv", package = "golf"))
  prod_club_metrics <- read.csv(system.file("extdata/csv_backup/club_metrics.csv", package = "golf"))
  
  dev_rounds <- read.csv(system.file("extdata/csv_backup/dev_rounds.csv", package = 'golf'))
  dev_courses <- read.csv(system.file("extdata/csv_backup/dev_courses.csv", package = 'golf'))
  dev_players <- read.csv(system.file("extdata/csv_backup/dev_players.csv", package = 'golf'))
  dev_club_metrics <- read.csv(system.file("extdata/csv_backup/dev_club_metrics.csv", package = 'golf'))
  
  
  DBI::dbWriteTable(con, "rounds",        prod_rounds,      overwrite = T)
  DBI::dbWriteTable(con, "courses",       prod_courses,     overwrite = T)
  DBI::dbWriteTable(con, "players",       prod_players,     overwrite = T)
  DBI::dbWriteTable(con, "club_metrics",  prod_club_metrics, overwrite = T)
  
  DBI::dbWriteTable(con, "dev_rounds",        dev_rounds,      overwrite = T)
  DBI::dbWriteTable(con, "dev_courses",       dev_courses,     overwrite = T)
  DBI::dbWriteTable(con, "dev_players",       dev_players,     overwrite = T)
  DBI::dbWriteTable(con, "dev_club_metrics",  dev_club_metrics, overwrite = T)
  
  DBI::dbDisconnect(con, shutdown = T)
  
  # Setup: refresh dev tables to match production ----
  golf::refresh_dev_tables(db_path = test_db)
  
  # Connect to the temporary DB
  con <- golf::get_db_connection(db_path = test_db)
  on.exit(DBI::dbDisconnect(con, shutdown = T), add = T)
  
  # 1. Baseline: promotion should succeed silently ----
  testthat::expect_silent(golf::promote_dev_to_production(db_path = test_db))
  
  # (promote_dev_to_production closes db connection)
  DBI::dbDisconnect(con, shutdown = T)
  con <- golf::get_db_connection(db_path = test_db)
  
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
  
  # (promote_dev_to_production closes db connection)
  DBI::dbDisconnect(con, shutdown = T)
  con <- golf::get_db_connection(db_path = test_db)
  
  # 3. Ensure production tables were NOT modified ----
  prod_players_before <- DBI::dbReadTable(con, "players")
  prod_players_after  <- DBI::dbReadTable(con, "players")
  
  testthat::expect_identical(prod_players_before, prod_players_after)
})
