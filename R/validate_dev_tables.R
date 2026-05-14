#' Validate dev tables before promotion to production
#'
#' @description
#' Performs structural and content validation on the development (staging)
#' versions of the production tables. Each dev table is evaluated
#' independently, and a per-table validity summary is produced.
#'
#' @param db_path optional path to a DuckDB database file; if supplied, the 
#' function uses this database instead of the package's default. Designed for
#' testing with temp writable databases
#'
#' Validation includes:
#' \itemize{
#'   \item All dev tables exist.
#'   \item Column names match the corresponding production tables.
#'   \item No malformed or missing dates in \code{dev_rounds}.
#'   \item No impossible gross scores.
#'   \item No duplicate rows under the appropriate composite keys:
#'     \itemize{
#'       \item \code{dev_rounds}: (date, course_name)
#'       \item \code{dev_players}: (ghin, date)
#'       \item \code{dev_club_metrics}: (hole, stroke, date, club)
#'       \item \code{dev_courses}: optional composite if available
#'             (e.g. (course_name, tee)); otherwise no uniqueness check.
#'     }
#' }
#'
#' The function does not stop at the first error. All tables are checked,
#' and a complete validation report is constructed. If any table fails
#' validation, the function stops *after* producing a detailed message.
#'
#' @return
#' A named logical vector indicating validity of each dev table.
#' The function stops with an error if any table is invalid.
#'
#' @import DBI
#' @import dplyr
#' @importFrom stats setNames
#' @export
validate_dev_tables <- function(db_path = NULL) {
  
  con <- golf::get_db_connection(db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  
  dev_tables <- c(
    "dev_rounds",
    "dev_courses",
    "dev_players",
    "dev_club_metrics"
  )
  
  prod_tables <- sub("^dev_", "", dev_tables)
  
  # initialize validation results -----
  results  <- stats::setNames(rep(TRUE, length(dev_tables)), dev_tables)
  messages <- vector("list", length(dev_tables))
  names(messages) <- dev_tables
  
  # 1. Check existence -----
  existing <- DBI::dbListTables(con)
  missing  <- setdiff(dev_tables, existing)
  
  if (length(missing) > 0) {
    for (tbl in missing) {
      results[[tbl]]  <- FALSE
      messages[[tbl]] <- paste0("Missing dev table: ", tbl)
    }
  }
  
  # 2. Schema checks ----
  for (i in seq_along(dev_tables)) {
    dev_tbl  <- dev_tables[i]
    prod_tbl <- prod_tables[i]
    
    if (!results[[dev_tbl]]) next
    
    dev_cols  <- DBI::dbListFields(con, dev_tbl)
    prod_cols <- DBI::dbListFields(con, prod_tbl)
    
    if (!identical(dev_cols, prod_cols)) {
      results[[dev_tbl]]  <- FALSE
      messages[[dev_tbl]] <- "Schema mismatch with production table."
    }
  }
  
  # 3. Content checks -----
  
  ## dev_rounds: dates, scores, composite duplicates (date, course_name)
  if (results[["dev_rounds"]]) {
    rounds <- DBI::dbReadTable(con, "dev_rounds")
    
    if (any(is.na(rounds$date))) {
      results[["dev_rounds"]]  <- FALSE
      messages[["dev_rounds"]] <- "Contains NA dates."
    }
    
    if (any(rounds$tot_gross < 40 | rounds$tot_gross > 200, na.rm = TRUE)) {
      results[["dev_rounds"]]  <- FALSE
      messages[["dev_rounds"]] <- "Contains impossible gross scores (<40 or >200)."
    }
    
    if (all(c("date", "course_name") %in% names(rounds))) {
      dupes <- rounds |>
        dplyr::count(.data$date, .data$course_name) |>
        dplyr::filter(n > 1)
      
      if (nrow(dupes) > 0) {
        results[["dev_rounds"]]  <- FALSE
        messages[["dev_rounds"]] <- "Duplicate rounds detected (date, course_name)."
      }
    }
  }
  
  ## dev_players: composite key (GHIN, date) must be unique
  if (results[["dev_players"]]) {
    players <- DBI::dbReadTable(con, "dev_players")
    
    if (all(c("GHIN", "date") %in% names(players))) {
      dupes <- players |>
        dplyr::count(.data$GHIN, .data$date) |>
        dplyr::filter(n > 1)
      
      if (nrow(dupes) > 0) {
        results[["dev_players"]]  <- FALSE
        messages[["dev_players"]] <- "Duplicate players detected (GHIN, date)."
      }
    }
  }
  
  ## dev_courses: optional composite uniqueness if clear key exists
  if (results[["dev_courses"]]) {
    courses <- DBI::dbReadTable(con, "dev_courses")
    
    # Example: if (course_name, tee) exists, enforce uniqueness on that pair.
    if (all(c("course_name", "tee") %in% names(courses))) {
      dupes <- courses |>
        dplyr::count(.data$course_name, .data$tee) |>
        dplyr::filter(n > 1)
      
      if (nrow(dupes) > 0) {
        results[["dev_courses"]]  <- FALSE
        messages[["dev_courses"]] <- "Duplicate courses detected (course_name, tee)."
      }
    }
    # If no obvious composite key is present, we skip uniqueness checks here.
  }
  
  ## dev_club_metrics: composite key (hole, stroke, date, club) must be unique
  if (results[["dev_club_metrics"]]) {
    cm <- DBI::dbReadTable(con, "dev_club_metrics")
    
    if (all(c("hole", "stroke", "date", "club") %in% names(cm))) {
      dupes <- cm |>
        dplyr::count(.data$hole, .data$stroke, .data$date, .data$club) |>
        dplyr::filter(n > 1)
      
      if (nrow(dupes) > 0) {
        results[["dev_club_metrics"]]  <- FALSE
        messages[["dev_club_metrics"]] <- "Duplicate club metrics (hole, stroke, date, club)."
      }
    }
  }
  
  # 4. Final reporting -----
  if (any(!results)) {
    bad <- names(results)[!results]
    
    msg <- paste0(
      "Validation failed for: ",
      paste(bad, collapse = ", "),
      "\n\nDetails:\n",
      paste(
        paste0(" - ", bad, ": ", unlist(messages[bad])),
        collapse = "\n"
      )
    )
    
    stop(msg)
  }
  
  invisible(results)
}
