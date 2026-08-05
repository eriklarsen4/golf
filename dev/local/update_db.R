# environment ----
library(golf)
library(tidyverse)
library(DBI)
library(duckdb)

# connect to the db
real_db <- fs::path_abs(paste0(getwd(), "../../../inst/extdata/golf.duckdb"))

# refresh the dev tables from current production tables
con <- golf::get_db_connection()
# con <- golf::get_db_connection(db_path = real_db)
golf::refresh_dev_tables(db_path = real_db)

# gather round info ----
round_course <- readline("Course name: ") #'Randolph North'
round_date <- readline("Round date (YYYY-MM-DD): ") #'2026-08-02'
round_tees <- readline("Tees: ") #'white'

cat("Enter 18 hole scores separated by spaces:\n")
hole_scores <- scan(what = integer(), quiet = T) #c(5, 5, 6, 4, 4, 3, 4, 4, 4,   5, 4, 5, 6, 5, 3, 5, 4, 6)

cat("Enter FIR values (18 boolean digits):\n")
FIRs <- scan(what = integer(), quiet = T) #c(rep(0, 4), 1, 0, 1, 0, 0,  rep(0, 9))

cat("Enter GIR values (18 boolean digits):\n")
GIRs <- scan(what = integer(), quiet = T)# c(rep(0, 3), rep(1, 6),  0, 0, 1, rep(0,3), 1, 0, 0) 

cat("Enter putts (18 numbers):\n")
putts_rec <- scan(what = integer(), quiet = T) # c(1, 1, 2, 2, 2, 2, 2, 3, 1,  1, 1, 3, 2, 2, 1, 2, 1, 3)

cat("Enter chips (18 numbers):\n")
chips_rec <- scan(what = integer(), quiet = T) #c(1, 1, 1, rep(0,5), 1,  2, 2, 0, 1, 1, 1, 0, 2, 1)

cat("Enter penalties (18 numbers):\n")
penalties_rec <- scan(what = integer(), quiet = T) #c(0, 1, rep(0,16))

cat("Enter the clubs used off each tee (18 strings, space-saparated):\n")
tee_clubs <- scan(what = character(), quiet = T) #c('D', 'D', 'D', 'D', 'D', 'SW', 'D', '6', 'D',  'D', '6', 'D', 'D', 'D', '9', 'D', 'D', 'D')

cat("Enter Handicap Index heading into the round:\n")
index <- as.numeric(readline("Handicap Index: ")) #9.8

# get the scorecard for the new round, ensuring hole-by-hole scores are filled
if ( length(hole_scores) > 0 ) {
  Card <- golf::get_course(course = round_course, date = round_date, tees = round_tees) 
  
  Card <- golf::log_score(Scorecard = Card,
                          hole_by_hole = hole_scores,
                          name = 'Erik Larsen',
                          index = index,
                          FIR = FIRs,
                          GIR = GIRs,
                          putts = putts_rec,
                          chips = chips_rec,
                          penalties = penalties_rec,
                          tee_club = tee_clubs)
}

# dev_courses formatting of Card ----
courses_Card <- Card |> 
  dplyr::select(course, tees, to_par, slope, course_rating, hole, yds, par, hole_handicap, gross, putts, penalties) |> 
  dplyr::distinct() |> 
  dplyr::rename(course_name = course)  |> 
  dplyr::mutate(hole = gsub(hole, 
                            pattern = 'hole_',
                            replacement = '') |> as.numeric())


# confirm data shape of tracked shots ----
club_metrics_df <- courses_Card |> 
  dplyr::mutate(date = as.character(round_date), .before = 1) |>  
  dplyr::mutate(tracked_shots = gross - putts - penalties) |> 
  dplyr::inner_join(
    DBI::dbGetQuery(conn = con, statement = paste0("SELECT DISTINCT * FROM dev_courses
                                                   WHERE course_name = '", round_course,"'
                                                   AND tees = '", round_tees,"';"))
  ) |> 
  dplyr::select(course_name, date, tees, hole, par, gross, tracked_shots)

# annotate club metrics ----
club_metrics_df_upload <- club_metrics_df |> 
  dplyr::mutate(
    club = purrr::map(tracked_shots, ~ rep(NA_character_, .x)),
    lie = purrr::map(tracked_shots, ~ rep(NA_character_, .x)),
    shot_type = purrr::map(tracked_shots, ~ rep(NA_character_, .x)),
    yds_to_target = purrr::map(tracked_shots, ~ rep(NA_real_, .x)),
    yds_traveled = purrr::map(tracked_shots, ~ rep(NA_real_, .x)),
    on_target = purrr::map(tracked_shots, ~ rep(NA_character_, .x)),
    miss_direction = purrr::map(tracked_shots, ~ rep(NA_character_, .x))
  ) |> 
  # tidyr::uncount(tracked_shots, .id = 'stroke') |>
  tidyr::unchop(cols = c(club, lie, shot_type, 
                         yds_to_target, yds_traveled, 
                         on_target, miss_direction)) |>
  dplyr::group_by(hole) |> 
  dplyr::mutate(stroke = dplyr::row_number(), .after = hole) |> 
  dplyr::ungroup()

# check shape of tracked shots for each hole (lengths)
club_metrics_df_upload |> 
  dplyr::group_by(hole) |> 
  dplyr::distinct(stroke, tracked_shots) |> 
  print(n = Inf)

check_length <- function(x, name, required_len) {
  if (length(x) != required_len) {
    stop( paste0("'", name, "' must have length ", required_len,
                 " but has length ", length(x), "."))
  }
  x
}

# manually annotate from Garmin Golf App log
club_choice <- scan(what = character(), quiet = T)
club_choice <- check_length(club_choice, "club_choice", club_metrics_df_upload |> nrow())

# club_choice <- c(
#   'D', 'GW', 'SW', 'GW',
#   'D', 'SW', 'P',
#   'D', '5', '7', 'PW',
#   'D', '9',
#   'D', 'SW',
#   'SW', 
#   'D', 'SW',
#   '6', 
#   'D', 'PW', 'GW',
#   
#   'D', '4', 'SW', 'PW',
#   '6', 'PW', 'PW',
#   'D', '8',
#   'D', '4', '9', 'PW',
#   'D', '5', 'LW', 
#   '9', 'GW',
#   'D', '5', '8',
#   'D', '7', 'LW',
#   'D', 'PW', 'LW'
# )

dist_to_target <- scan(what = integer(), quiet = T)
dist_to_target <- check_length(dist_to_target, "dist_to_target", club_metrics_df_upload |> nrow())

# dist_to_target <- c(
#   270, 131, 98, 19,
#   270, 112, 11,
#   270, 50, 170, 20,
#   270, 153,
#   270, 93,
#   83,
#   270, 87,
#   210,
#   270, 166, 23,
#   
#   270, 140, 18, 13,
#   191, 50, 15,
#   270, 163,
#   270, 220, 152, 23,
#   270, 100, 30, 
#   173, 26,
#   270, 210, 160,
#   270, 65, 20,
#   270, 141, 27
# )

yds <- scan(what = integer(), quiet = T)
yds <- check_length(yds, "yds", club_metrics_df_upload |> nrow() )

# yds <- c(
#   228, 34, 85, 23,
#   263, 114, 12,
#   297, 57, 191, 26,
#   267, 150,
#   325, 96,
#   92,
#   285, 87,
#   192,
#   268, 153, 21,
#   
#   284, 122, 5, 20, 
#   221, 36, 14,
#   206, 171,
#   266, 114, 130, 18,
#   287, 113, 30,
#   164, 31,
#   261, 121, 160,
#   320, 79, 20,
#   265, 165, 20
# )

lie_type <- scan(what = character(), quiet = T)
lie_type <- check_length(lie_type, "lie_type", club_metrics_df_upload |> nrow() )

# lie_type <- c(
#   'tee', 'sand', 'rough', 'fairway',
#   'tee', 'fairway', 'fairway',
#   'tee', 'rough', 'fairway', 'fairway',
#   'tee', 'rough',
#   'tee', 'fairway',
#   'tee',
#   'tee', 'fairway',
#   'tee',
#   'tee', 'fairway', 'rough',
#   
#   'tee', 'rough', 'fairway', 'fairway',
#   'tee', 'rough', 'fairway',
#   'tee', 'rough',
#   'tee', 'rough', 'rough', 'fairway',
#   'tee', 'rough', 'rough',
#   'tee', 'rough',
#   'tee', 'rough', 'fairway',
#   'tee', 'rough', 'sand',
#   'tee', 'rough', 'sand'
# )

target_status <- scan(what = character(), quiet = T)
target_status <- check_length(target_status, "target_status", club_metrics_df_upload |> nrow() )

# target_status <- c(
#   'no', 'no', 'yes', 'yes',
#   'no', 'no', 'yes',
#   'no', 'yes', 'no', 'yes',
#   'no', 'yes',
#   'yes', 'yes',
#   'yes',
#   'yes', 'yes',
#   'yes',
#   'no', 'yes', 'yes',
#   
#   'no', 'yes', 'no', 'yes',
#   'no', 'no', 'yes',
#   'no', 'yes',
#   'no', 'no', 'no', 'yes',
#   'no', 'no', 'yes',
#   'no', 'yes',
#   'no', 'no', 'yes',
#   'no', 'no', 'yes',
#   'no', 'no', 'yes'
# )

location <- scan(what = character(), quiet = T)
location <- check_length(location, "location", club_metrics_df_upload |> nrow() )

# location <- c(
#   'left', 'short', 'on_target', 'on_target',
#   'left', 'left', 'on_target',
#   'left', 'on_target', 'long', 'on_target',
#   'right', 'on_target',
#   'on_target', 'on_target',
#   'on_target',
#   'on_target', 'on_target',
#   'on_target',
#   'right', 'on_target', 'on_target',
#   
#   'right', 'on_target', 'short', 'on_target',
#   'long', 'short', 'on_target',
#   'left', 'on_target',
#   'right', 'short', 'short', 'on_target',
#   'right', 'long', 'on_target',
#   'right', 'on_target',
#   'right', 'short', 'on_target',
#   'long', 'long', 'on_target',
#   'right', 'long', 'on_target'
# )

type_of_shot <- scan(what = character(), quiet = T)
type_of_shot <- check_length(type_of_shot, "type_of_shot", club_metrics_df_upload |> nrow() )

# type_of_shot <- c(
#   'tee', 'fwbunker', 'punch', 'chip',
#   'tee', 'full', 'chip',
#   'tee', 'punch', 'full', 'chip',
#   'tee', 'full',
#   'tee', 'choked',
#   'tee',
#   'tee', 'choked',
#   'tee',
#   'tee', 'full', 'chip',
#   
#   'tee', 'punch', 'chip', 'chip',
#   'tee', 'chip', 'chip',
#   'tee', 'full',
#   'tee', 'full', 'full', 'chip',
#   'tee', 'punch', 'chip',
#   'tee', 'chip',
#   'tee', 'full', 'full',
#   'tee', 'punch', 'gsbunker',
#   'tee', 'full', 'gsbunker'
# )

required_len <- club_metrics_df_upload |> nrow()

vectors <- list(
  club = club_choice,
  yds_to_target = dist_to_target,
  yds_traveled = yds,
  lie = lie_type,
  on_target = target_status,
  miss_direction = location,
  shot_type = type_of_shot
)

for (nm in names(vectors)) {
  check_length(vectors[[nm]], nm, required_len)
}

club_metrics_df_upload <- club_metrics_df_upload |> 
  dplyr::mutate(
    club = club_choice,
    yds_to_target = dist_to_target,
    yds_traveled = yds,
    lie = lie_type,
    on_target = target_status,
    miss_direction = location,
    shot_type = type_of_shot
  )

club_metrics_df_upload

# players table -----
if ( DBI::dbGetQuery(conn = con, statement = paste0("SELECT DISTINCT date FROM rounds ORDER BY date DESC LIMIT 1;")) |> 
     dplyr::distinct(date) |> 
     unlist() %>% 
     lubridate::as_date(.) |> 
     as.character() < round_date &
     
     length(hole_scores) > 0
) {
  
  DBI::dbAppendTable(conn = con,
                     name = 'dev_players',
                     value = players <- Card |> 
                       dplyr::select(player_id, player_name, GHIN, index, date) |> 
                       dplyr::distinct() |> 
                       dplyr::rename(handicap_index = index) |> 
                       dplyr::mutate(date = as.character(date))
  )
  
}
# courses table ----
if ( DBI::dbGetQuery(conn = con, statement = paste0("SELECT DISTINCT date FROM rounds ORDER BY date DESC LIMIT 1;")) |> 
     dplyr::distinct(date) |> 
     unlist() %>% 
     lubridate::as_date(.) |> 
     as.character() < round_date &
     
     length(hole_scores) > 0
) {
  DBI::dbAppendTable(conn = con,
                     name = 'dev_courses',
                     value = course <- Card |> 
                       dplyr::select(course, tees, to_par, slope, course_rating, hole, yds, par, hole_handicap) |> 
                       dplyr::distinct() |> 
                       dplyr::rename(course_name = course)  |> 
                       dplyr::mutate(hole = gsub(hole, 
                                                 pattern = 'hole_',
                                                 replacement = '') |> as.numeric())
  )
}

# rounds table ----
if ( DBI::dbGetQuery(conn = con, statement = paste0("SELECT DISTINCT date FROM rounds ORDER BY date DESC LIMIT 1;")) |> 
     dplyr::distinct(date) |> 
     unlist() %>% 
     lubridate::as_date(.) |> 
     as.character() < round_date &
     
     length(hole_scores) > 0
) {
  DBI::dbAppendTable(conn = con,
                     name = 'dev_rounds',
                     value = Card |> 
                       dplyr::select(-to_par, -slope, -course_rating, -yds, -par) |> 
                       dplyr::distinct() |>
                       dplyr::rename(handicap_index = index) |> 
                       dplyr::rename(course_name = course) |> 
                       dplyr::mutate(date = as.character(date))  |> 
                       dplyr::mutate(hole = gsub(hole, 
                                                 pattern = 'hole_',
                                                 replacement = '') |> as.numeric()) |> 
                       dplyr::group_by(course_name, date) |> 
                       dplyr::arrange(hole) |> 
                       dplyr::ungroup() |> 
                       dplyr::relocate(c(tot_gross, tot_net), .after = IN_net) |> 
                       dplyr::relocate(course_handicap, .after = tees)
  )
}

# club metrics table -----
if ( DBI::dbGetQuery(conn = con, statement = paste0("SELECT DISTINCT date FROM club_metrics ORDER BY date DESC LIMIT 1;")) |> 
     dplyr::distinct(date) |> 
     unlist() %>% 
     lubridate::as_date(.) |> 
     as.character() < round_date &&
     
     length(hole_scores) > 0
) {
  
  DBI::dbAppendTable(conn = con,
                     name = 'dev_club_metrics',
                     value = club_metrics_df_upload |> 
                       dplyr::relocate(stroke, .after = gross) |>
                       dplyr::relocate(lie, .after = stroke) |> 
                       dplyr::relocate(shot_type, .after = miss_direction) |> 
                       dplyr::select(-tracked_shots) |> 
                       dplyr::mutate(dplyr::across(c(dplyr::contains("yds")), ~as.numeric(.x))) |> 
                       dplyr::mutate(date = lubridate::as_date(date))
  )
}

# validate tables -----
golf::validate_dev_tables(db_path = real_db)

# DBI::dbGetQuery(conn = con, statement = paste0("SELECT DISTINCT * FROM rounds;"))
# DBI::dbGetQuery(conn = con, statement = paste0("SELECT DISTINCT * FROM club_metrics;")) |> colnames()
# club_metrics_df_upload |> 
#   dplyr::relocate(stroke, .after = gross) |>
#   dplyr::relocate(lie, .after = stroke) |> 
#   dplyr::relocate(shot_type, .after = miss_direction) |> 
#   dplyr::select(-tracked_shots) |> 
#   dplyr::mutate(dplyr::across(c(dplyr::contains("yds")), ~as.numeric(.x))) |>  colnames()

# promote dev to production tables ----
golf::promote_dev_to_production(db_path = real_db)

# run the skill pipeline----
if (DBI::dbGetQuery(conn = con, statement = paste0("SELECT COUNT(*) AS n FROM dev_rounds WHERE date = '", round_date, "';"))$n > 0L) {
  golf::run_skill_pipeline(db_path = real_db)
}

# call the run_etl.R script ----
source(paste0(getwd(), '/dev/local/run_etl.R'))