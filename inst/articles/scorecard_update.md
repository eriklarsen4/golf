## Overview

This markdown shows the data logging process of each round, and shows
performance trends and metric averages

## Set-Up Environment

### Attach Packages

``` r
library(golf)
library(tidyverse)
library(DBI)
library(duckdb)
```

## Record New Scorecard

### Input the Scores Data

``` r
# required inputs to fill out the scorecard

round_course <- 'Randolph North'
round_date <- '2026-07-03'
round_tees <- 'white'

hole_scores <- c(5, 6, 6, 5, 4, 3, 4, 4, 4,
                 6, 3, 4, 5, 5, 3, 5, 4, 5)

FIRs <- c(rep(0,9),
          rep(0,3), 1, rep(0,5))

GIRs <- c(rep(0,4), 1, 1, 0, 0, 1,
          rep(0,3), 1, rep(0, 3), 1, 0) 

putts_rec <- c(1, 2, 2, 2, 2, 2, 1, 2, 1,
               2, 1, 1, 2, 2, 1, 1, 2, 2)

chips_rec <- c(2, 1, 1, 1, 0, 0, 1, 1, 1,
               2, 1, 1, 0, 1, 1, 0, 0, 1)

penalties_rec <- c(rep(0, 18))

tee_clubs <- c('D', 'D', 'D', 'D', 'D', 'GW', 'D', '5', 'D',
               'D', '6', 'D', 'D', 'D', '8', 'D', 'D', 'D')

index <- 10.6
```

### Specify Course and Tees

``` r
# get the scorecard for the new round, ensuring hole-by-hole scores are filled
if ( length(hole_scores) > 0 ) {
 Card <- golf::get_course(course = round_course, date = round_date, tees = round_tees) 
}
```

### Get Scoring Metrics

``` r
# fill the scorecard for the new round, ensuring hole-by-hole scores are filled
if ( length(hole_scores) > 0 ) {
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
```

## Update the db

### Update the Players Table

``` r
con <- golf::get_db_connection()

# if the logged data is newer than the last logged round in the 'rounds' table, 
# update the 'players' table

if ( DBI::dbGetQuery(conn = con, statement = paste0("SELECT DISTINCT date FROM rounds ORDER BY date DESC LIMIT 1;")) |> 
     dplyr::distinct(date) |> 
     unlist() %>% 
     lubridate::as_date(.) |> 
     as.character() < round_date &
     
     length(hole_scores) > 0
     ) {

  DBI::dbAppendTable(conn = con,
                   name = 'players',
                   value = players <- Card |> 
                     dplyr::select(GHIN, index, date) |> 
                     dplyr::distinct() |> 
                     dplyr::rename(handicap_index = index) |> 
                     dplyr::mutate(date = as.character(date))
                   )
  
}
```

### Update the Courses Table

``` r
con <- golf::get_db_connection()

# if the logged data is newer than the last logged round in the 'rounds' table, 
# update the 'courses' table

if ( DBI::dbGetQuery(conn = con, statement = paste0("SELECT DISTINCT date FROM rounds ORDER BY date DESC LIMIT 1;")) |> 
     dplyr::distinct(date) |> 
     unlist() %>% 
     lubridate::as_date(.) |> 
     as.character() < round_date &
     
     length(hole_scores) > 0
     ) {
  DBI::dbAppendTable(conn = con,
                   name = 'courses',
                   value = course <- Card |> 
                     dplyr::select(course, tees, to_par, slope, course_rating, hole, yds, par, hole_handicap) |> 
                     dplyr::distinct() |> 
                     dplyr::rename(course_name = course) |> 
                     dplyr::mutate(hole = gsub(hole, 
                                               pattern = 'hole_',
                                               replacement = '') |> as.numeric())
                   )
}
```

### Update the Rounds Table

``` r
con <- golf::get_db_connection()

# if the logged data is newer than the last logged round in the 'rounds' table, 
# update the 'rounds' table

if ( DBI::dbGetQuery(conn = con, statement = paste0("SELECT DISTINCT date FROM rounds ORDER BY date DESC LIMIT 1;")) |> 
     dplyr::distinct(date) |> 
     unlist() %>% 
     lubridate::as_date(.) |> 
     as.character() < round_date &
     
     length(hole_scores) > 0
     ) {
  DBI::dbAppendTable(conn = con,
                   name = 'rounds',
                   value = Card |> 
                     dplyr::select(-to_par, -slope, -course_rating, -yds, -par) |> 
                     dplyr::distinct() |>
                     dplyr::rename(handicap_index = index) |> 
                     dplyr::rename(course_name = course) |> 
                     dplyr::mutate(date = as.character(date))  |> 
                     dplyr::mutate(hole = gsub(hole, 
                                               pattern = 'hole_',
                                               replacement = '') |> as.numeric())
                   )
}
```

### Get Club Metrics df

``` r
# create a dataframe, specifying the shape for tracked shots on each hole
  # -> all strokes from off the green

club_metrics_df <- golf::get_tracked_shots_data_shape(round_date = round_date) |> 
  dplyr::arrange(hole)
```

### Annotate Club Metrics

``` r
# manually annotate from Garmin Golf App log
club_choice <- c(
  'D', 'GW', 'GW', 'PW',
  'D', '4', 'GW', 'PW',
  'D', '3W', '5', 'PW',
  'D', '8', 'GW',
  'D', 'GW',
  'GW',
  'D', '4', 'PW',
  '5', 'PW',
  'D', '4', 'SW',
  
  'D', '5', 'SW', 'PW',
  '6', 'PW',
  'D', '5', 'P',
  'D', '6', 'PW',
  'D', '4', 'SW',
  '8', 'PW',
  'D', '4', '4', '4',
  'D', 'SW',
  'D', '7', 'GW'
)

dist_to_target <- c(
  270, 89, 21, 12,
  270, 195, 132, 14,
  270, 250, 179, 26,
  270, 168, 21,
  270, 122, 
  112, 
  270, 82, 20,
  210, 9,
  270, 210, 18,
  
  270, 133, 48, 17,
  179, 19,
  270, 106, 13,
  270, 189, 16,
  270, 169, 26,
  168, 11,
  270, 238, 224, 186,
  270, 89,
  270, 168, 28
)

yds <- c(
  275, 110, 9, 12,
  168, 64, 144, 10,
  240, 100, 202, 16,
  257, 178, 24,
  288, 127,
  128,
  298, 98, 16,
  219, 13,
  234, 194, 19,
  
  295, 85, 32, 12,
  177, 14,
  289, 119, 9,
  288, 192, 11,
  245, 151, 21,
  169, 10,
  266, 16, 38, 186,
  282, 77,
  258, 195, 20
)
lie_type <- c(
  'tee', 'rough', 'rough', 'fairway',
  'tee', 'rough', 'rough', 'rough',
  'tee', 'fairway', 'rough', 'rough',
  'tee', 'rough', 'rough',
  'tee', 'rough',
  'tee', 
  'tee', 'rough', 'rough',
  'tee', 'rough',
  'tee', 'rough', 'fairway',
  
  'tee', 'rough', 'fairway', 'fairway',
  'tee', 'rough',
  'tee', 'rough', 'fairway',
  'tee', 'fairway', 'rough',
  'tee', 'rough', 'fairway',
  'tee', 'rough',
  'tee', 'rough', 'rough', 'rough',
  'tee', 'rough',
  'tee', 'rough', 'rough'
)

target_status <- c(
  'no', 'no', 'no', 'yes',
  'no', 'no', 'no', 'yes',
  'no', 'no', 'no', 'yes',
  'no', 'no', 'yes',
  'no', 'yes',
  'yes',
  'no', 'no', 'yes',
  'no', 'yes',
  'no', 'yes', 'yes',
  
  'no', 'yes', 'no', 'yes',
  'no', 'yes',
  'no', 'no', 'yes',
  'yes', 'no', 'yes',
  'no', 'yes', 'yes',
  'no', 'yes',
  'no', 'no', 'no', 'yes',
  'no', 'yes',
  'no', 'no', 'yes'
)

location <- c(
  'right', 'long', 'short', 'on_target',
  'left', 'short', 'long', 'on_target',
  'right', 'short', 'long', 'on_target',
  'right', 'long', 'on_target',
  'right', 'on_target',
  'on_target',
  'long', 'long', 'on_target',
  'long', 'on_target',
  'left', 'on_target', 'on_target',
  
  'right', 'on_target', 'short', 'on_target',
  'right', 'on_target',
  'right', 'long', 'on_target',
  'on_target', 'long', 'on_target',
  'right', 'on_target', 'on_target',
  'right', 'on_target',
  'right', 'short', 'short', 'on_target',
  'right', 'on_target',
  'right', 'long', 'on_target'
)

type_of_shot <- c(
  'tee', 'choked', 'chip', 'chip',
  'tee', 'punch', 'full', 'chip',
  'tee', 'full', 'full', 'chip',
  'tee', 'full', 'chip',
  'tee', 'full',
  'tee',
  'tee', 'punch', 'chip',
  'tee', 'chip',
  'tee', 'full', 'chip',
  
  'tee', 'punch', 'chip', 'chip',
  'tee', 'chip',
  'tee', 'punch', 'chip',
  'tee', 'full', 'chip',
  'tee', 'punch', 'chip',
  'tee', 'chip',
  'tee', 'punch', 'punch', 'full',
  'tee', 'full',
  'tee', 'full', 'chip'
)
```

### Get Shot Metrics

``` r
con <- golf::get_db_connection()

# if the data is newer than the last round in the 'club_metrics' table, append
  # the new annotations (expands from 18 rows by the number determined from the 
  # get_tracked_shots_data_shape function)

if ( DBI::dbGetQuery(conn = con, statement = paste0("SELECT DISTINCT date FROM club_metrics ORDER BY date DESC LIMIT 1;")) |> 
     dplyr::distinct(date) |> 
     unlist() %>% 
     lubridate::as_date(.) |> 
     as.character() < round_date &
     
     length(hole_scores) > 0
) {
  length(club_choice)
  club_metrics <- golf::harmonize_club_metrics(club_metrics = club_metrics_df,
                                               club_choice = club_choice,
                                               distance_to_target = dist_to_target,
                                               distance_traveled = yds,
                                               lie_type = lie_type,
                                               target_status = target_status,
                                               location = location,
                                               type_of_shot = type_of_shot
                                               )
  club_metrics <- club_metrics |> 
    dplyr::mutate(dplyr::across(c(dplyr::contains("yds")), ~as.numeric(.x)))
  head(club_metrics)
}

club_metrics_df |> dplyr::select(tracked_shots) |> dplyr::mutate(sum(tracked_shots))
```

    ##    tracked_shots sum(tracked_shots)
    ## 1              4                 52
    ## 2              4                 52
    ## 3              4                 52
    ## 4              3                 52
    ## 5              2                 52
    ## 6              1                 52
    ## 7              3                 52
    ## 8              2                 52
    ## 9              3                 52
    ## 10             4                 52
    ## 11             2                 52
    ## 12             3                 52
    ## 13             3                 52
    ## 14             3                 52
    ## 15             2                 52
    ## 16             4                 52
    ## 17             2                 52
    ## 18             3                 52

### Update the Club Metrics Table

``` r
con <- golf::get_db_connection()

# if the data is newer than the last round in the 'club_metrics' table, append
  # the new data to the table

if ( DBI::dbGetQuery(conn = con, statement = paste0("SELECT DISTINCT date FROM club_metrics ORDER BY date DESC LIMIT 1;")) |> 
     dplyr::distinct(date) |> 
     unlist() %>% 
     lubridate::as_date(.) |> 
     as.character() < round_date &
     
     length(hole_scores) > 0
     ) {
  DBI::dbAppendTable(conn = con,
                   name = 'club_metrics',
                   value = club_metrics |> 
                     dplyr::mutate(date = as.character(date))
                   )
}
```

``` r
# clean the global environment
rm(list = ls()[which(grepl(ls(), pattern= 'con|round_course|round_date|index|real_db')==F)])
```

## Summarize Metrics

### Gather and Format

Gather and format from the database

``` r
con <- golf::get_db_connection()

# get the hole-by-hole scores from each round in the database

  # join the hole-by-hole scores from the 'rounds' table to the 'courses' table
    # need each hole's par rating 
    # need each course's course_rating from the 'courses' table
  
  # join the 'rounds' and 'courses' tables to the 'players' table
    # need handicap_index from the 'players' table

scores <- DBI::dbGetQuery(conn = con, statement = paste0(
  "SELECT DISTINCT r.*, c.par, c.course_rating FROM rounds r
  INNER JOIN courses c
  ON c.tees = r.tees
  AND c.course_name = r.course_name
  AND c.hole = r.hole
  INNER JOIN players p
  ON r.GHIN = p.GHIN
  AND r.handicap_index = p.handicap_index
  AND r.date = p.date;"
)) |> 
  dplyr::mutate(date = lubridate::as_date(date)) |> # convert strings to date's
  dplyr::relocate(par, .after = hole) |> 
  dplyr::relocate(course_rating, .after = tees) |>
  dplyr::group_by(date) |> 
  dplyr::arrange(desc(date), hole) |> 
  dplyr::ungroup()
```

``` r
con <- golf::get_db_connection()

# get stroke metrics from the 'club_metrics' table

stroke_quality <- DBI::dbGetQuery(conn = con, statement = paste0(
  "SELECT DISTINCT * FROM club_metrics;"
)) |> 
  dplyr::mutate(date = lubridate::as_date(date)) |> # convert strings to date's
  dplyr::group_by(date) |> 
  dplyr::arrange(desc(date), hole, stroke) |> 
  dplyr::ungroup()
```

    ## [1] "BEFORE TRY BLOCK"
    ## [1] "TRY BLOCK START"

    ## [1] "TABLES INSIDE TRY, AFTER DEV WRITES:"
    ##  [1] "club_metrics"          "courses"               "dev_club_metrics"      "dev_courses"           "dev_players"           "dev_predictions_round" "dev_rounds"           
    ##  [8] "dev_scores_sum"        "pipeline_run_log"      "players"               "predictions_round"     "rounds"               
    ## [1] "INSIDE TRY BLOCK - END"

### Compute Metrics

Compute standard metrics

``` r
# define metrics
scores_sum <- scores |>
  dplyr::mutate(date_course = paste0(date,
                                     '\n',
                                     course_name, '\n',
                                     handicap_index),
                chips = dplyr::case_when(
                  grepl(date, pattern = '07-13') ~ NA_real_, TRUE ~ chips),
                `chips+putts` = chips+putts,
                FIR_opps = dplyr::case_when(par > 3 ~ 1, TRUE ~ NA),
                `Iron FIRs` = dplyr::case_when(par > 3 &
                                                 grepl(tee_club, pattern = '4|5') &
                                                 FIR == 1 ~ 1,
                                               TRUE ~ 0),
                `Iron FIR opps` = dplyr::case_when(par > 3 &
                                                     grepl(tee_club, pattern = '4|5') ~ 1,
                                                   TRUE ~ 0),
                `Driver FIRs` = dplyr::case_when(par > 3 &
                                                   tee_club == 'D' &
                                                   FIR == 1 ~ 1,
                                                 TRUE ~ 0),
                `Driver FIR opps` = dplyr::case_when(par > 3 &
                                                     tee_club == 'D' ~ 1,
                                                   TRUE ~ 0),
                `Par 3 GIRs` = dplyr::case_when(par == 3 & 
                                                  GIR == 1 ~ 1,
                                                TRUE ~ 0),
                greenie_putts = dplyr::case_when(GIR == 1 ~ putts, TRUE ~ NA_real_),
                updown_conv = dplyr::case_when(GIR == 0 &
                                                 par == gross ~ 1,
                                               TRUE ~ 0),
                updown_opps = dplyr::case_when(GIR == 0 &
                                                chips > 0 ~ 1,
                                              TRUE ~ 0)
                ) |> 
  dplyr::rename(`Handicap Index` = handicap_index)

# summarize by round
scores_sum <- scores_sum |> 
  dplyr::group_by(date, date_course, course_rating, `Handicap Index`) |> 
  dplyr::summarize(
                   FIRs = sum(FIR, na.rm = F),
                   `Iron FIRs` = sum(`Iron FIRs`, na.rm = T),
                   `Iron FIR%` = round(((sum(`Iron FIRs`, na.rm = T)/sum(`Iron FIR opps`, na.rm = T))*100), 1),
                   `Driver FIRs` = sum(`Driver FIRs`, na.rm = T),
                   `Driver FIR%` = round(((sum(`Driver FIRs`, na.rm = T)/sum(`Driver FIR opps`, na.rm = T))*100), 1),
                   `FIR%` = round((sum(FIRs, na.rm = T)/sum(FIR_opps, na.rm = T))*100, 1),
                   GIRs = sum(GIR, na.rm = F),
                   `Par 3 GIRs` = sum(`Par 3 GIRs`, na.rm = T),
                   `GIR%` = round((sum(GIRs, na.rm = T)/18)*100, 1),
                   putts = sum(putts, na.rm = F),
                   `Avg GIR putts` = round(mean(greenie_putts, na.rm = T), 2),
                   chips = sum(chips, na.rm = F),
                   `chips+putts` = sum(`chips+putts`, na.rm = F),
                   `UpDown%` = round(((sum(updown_conv, na.rm = T)/sum(updown_opps, na.rm = T))*100), 1),
                   pars = sum(is_gross_par, na.rm = F),
                   birdies = sum(is_gross_birdie, na.rm = F),
                   bogies = sum(is_gross_bogey, na.rm = F),
                   `doubles+` = sum(is_gross_bogey_worse, na.rm = F),
                   penalties = sum(penalties, na.rm = T),
                   `Gross Score` = mean(tot_gross, na.rm = F),
                   `Net Score` = mean(tot_net, na.rm = F)) %>%
  dplyr::mutate(dplyr::across(c(`Iron FIRs`, `Driver FIRs`,
                                `Iron FIR%`, `Driver FIR%`, `FIR%`),
                              ~dplyr::if_else(is.na(FIRs), NA, .)),
                
                dplyr::across(c(`Par 3 GIRs`, `Avg GIR putts`,
                                `UpDown%`, `GIR%`), 
                              ~dplyr::if_else(is.na(GIRs), NA, .)),
                
                `chips+putts` = dplyr::case_when(is.na(chips) ~ NA_real_,
                                                 TRUE ~ `chips+putts`)) %>% 
  
  dplyr::mutate(
    
    `UpAndDown%` = dplyr::case_when(
      
      grepl(date, pattern = '07-13|09-21') ~ NA, TRUE ~ `UpDown%`),
    
    `Iron FIR%` = dplyr::case_when(`Iron FIR%` == NaN ~ 0.0, TRUE ~ `Iron FIR%`))
```

``` r
head(scores_sum |> 
       dplyr::arrange(desc(date)))
```

    ## # A tibble: 6 × 26
    ## # Groups:   date, date_course, course_rating [6]
    ##   date       date_course        course_rating `Handicap Index`  FIRs `Iron FIRs` `Iron FIR%` `Driver FIRs` `Driver FIR%` `FIR%`  GIRs `Par 3 GIRs` `GIR%` putts `Avg GIR putts` chips `chips+putts`
    ##   <date>     <chr>                      <dbl>            <dbl> <int>       <dbl>       <dbl>         <dbl>         <dbl>  <dbl> <int>        <dbl>  <dbl> <int>           <dbl> <dbl>         <dbl>
    ## 1 2026-07-03 "2026-07-03\nRand…          69.8             10.6     1           0       NaN               1           7.1    7.1     5            1   27.8    29            1.8     15            44
    ## 2 2026-06-28 "2026-06-28\nSilv…          68               10.4     4           0       NaN               3          25     30.8     4            0   22.2    34            2.5     16            50
    ## 3 2026-06-28 "2026-06-28\nSilv…          68.9             10.4     4           0       NaN               3          25     30.8     4            0   22.2    34            2.5     16            50
    ## 4 2026-06-21 "2026-06-21\nRand…          69.8             10.4     6           1       100               5          38.5   42.9     9            1   50      38            2.44    12            50
    ## 5 2026-06-07 "2026-06-07\nRand…          69.8             11       4           1       100               3          23.1   28.6     6            1   33.3    27            1.33    15            42
    ## 6 2026-05-31 "2026-05-31\nFred…          68.6             11       3           1        12.5             2          33.3   21.4     5            2   27.8    34            2.2     13            47
    ## # ℹ 9 more variables: `UpDown%` <dbl>, pars <int>, birdies <int>, bogies <int>, `doubles+` <int>, penalties <int>, `Gross Score` <dbl>, `Net Score` <dbl>, `UpAndDown%` <dbl>

### View Metrics

Separate and view metrics

#### Scoring Metrics

Scores and Handicap

``` r
scoring_metrics <- scores_sum |> 
  dplyr::select(`Handicap Index`, `Gross Score`, `Net Score`)
head(scoring_metrics |> 
       dplyr::arrange(desc(date)))
```

    ## # A tibble: 6 × 6
    ## # Groups:   date, date_course, course_rating [6]
    ##   date       date_course                        course_rating `Handicap Index` `Gross Score` `Net Score`
    ##   <date>     <chr>                                      <dbl>            <dbl>         <dbl>       <dbl>
    ## 1 2026-07-03 "2026-07-03\nRandolph North\n10.6"          69.8             10.6            81          72
    ## 2 2026-06-28 "2026-06-28\nSilverbell\n10.4"              68               10.4            89          80
    ## 3 2026-06-28 "2026-06-28\nSilverbell\n10.4"              68.9             10.4            89          80
    ## 4 2026-06-21 "2026-06-21\nRandolph North\n10.4"          69.8             10.4            83          75
    ## 5 2026-06-07 "2026-06-07\nRandolph North\n11"            69.8             11              77          68
    ## 6 2026-05-31 "2026-05-31\nFred Enke\n11"                 68.6             11              90          82

#### Stroke Metrics

In golf, every hole has a `par`– an average number of strokes taken to
get the ball in the hole

There are (almost always), three different `par`s on every course:

- `par 3`s
- `par 4`s
- `par 5`s

A shorthand to determine how a golfer performed across holes is to rate
how many strokes *relative to par* they were (-1, -2, +1, +2, etc)

These numbers are given names:

- `0 = par`
- `-2 = eagle`
- `-1 = birdie`
- `+1 = bogey`
- `+2 = double bogey`

I quantify these below:

``` r
stroke_metrics <- scores_sum |> 
  dplyr::select(`doubles+`, bogies, pars, birdies)
head(stroke_metrics |> 
       dplyr::arrange(desc(date)))
```

    ## # A tibble: 6 × 7
    ## # Groups:   date, date_course, course_rating [6]
    ##   date       date_course                        course_rating `doubles+` bogies  pars birdies
    ##   <date>     <chr>                                      <dbl>      <int>  <int> <int>   <int>
    ## 1 2026-07-03 "2026-07-03\nRandolph North\n10.6"          69.8          2      6     9       1
    ## 2 2026-06-28 "2026-06-28\nSilverbell\n10.4"              68            5      7     6       0
    ## 3 2026-06-28 "2026-06-28\nSilverbell\n10.4"              68.9          5      7     6       0
    ## 4 2026-06-21 "2026-06-21\nRandolph North\n10.4"          69.8          1      9     8       0
    ## 5 2026-06-07 "2026-06-07\nRandolph North\n11"            69.8          1      8     5       3
    ## 6 2026-05-31 "2026-05-31\nFred Enke\n11"                 68.6          5      7     5       1

#### Around-the-Green Metrics

`Chips`: + strokes around the green taken to get onto the green

`Putts`: + strokes taken with the putter on the green

`Avg GIR putts`: + Average \# of putts on holes where the ball was hit
on to the green within 2 strokes of par (green in regulation, `GIR`)

`UpDown%` (aka `Scramble%`): + \# of holes without a `GIR` but par was
made / \# of holes without a `GIR`

``` r
atg_metrics <- scores_sum |> 
  dplyr::select(chips, `chips+putts`, `UpAndDown%`, putts, `Avg GIR putts`)
head(atg_metrics |> 
       dplyr::arrange(desc(date)))
```

    ## # A tibble: 6 × 8
    ## # Groups:   date, date_course, course_rating [6]
    ##   date       date_course                        course_rating chips `chips+putts` `UpAndDown%` putts `Avg GIR putts`
    ##   <date>     <chr>                                      <dbl> <dbl>         <dbl>        <dbl> <int>           <dbl>
    ## 1 2026-07-03 "2026-07-03\nRandolph North\n10.6"          69.8    15            44         41.7    29            1.8 
    ## 2 2026-06-28 "2026-06-28\nSilverbell\n10.4"              68      16            50         25      34            2.5 
    ## 3 2026-06-28 "2026-06-28\nSilverbell\n10.4"              68.9    16            50         25      34            2.5 
    ## 4 2026-06-21 "2026-06-21\nRandolph North\n10.4"          69.8    12            50         25      38            2.44
    ## 5 2026-06-07 "2026-06-07\nRandolph North\n11"            69.8    15            42         25      27            1.33
    ## 6 2026-05-31 "2026-05-31\nFred Enke\n11"                 68.6    13            47         20      34            2.2

#### Ball Striking Metrics

Approach and tee accuracy

`GIR (green in regulation)`: + hole where the ball was hit on to the
green within 2 strokes of par

`FIR (fairway in regulation)`: + hole where the ball was hit on to the
fairway from the tee box (only on par 4’s and par 5’s)

`Iron FIR`: + hole where an iron was used off the tee for a `FIR`

`Driver FIR`: + hole where driver was used off the tee for a `FIR`

``` r
ball_striking_metrics <- scores_sum |> 
  dplyr::select(GIRs, `GIR%`, `Par 3 GIRs`,
                FIRs, `FIR%`, `Iron FIRs`, `Iron FIR%`,
                `Driver FIRs`, `Driver FIR%`)
head(ball_striking_metrics |> 
       dplyr::arrange(desc(date)))
```

    ## # A tibble: 6 × 12
    ## # Groups:   date, date_course, course_rating [6]
    ##   date       date_course                        course_rating  GIRs `GIR%` `Par 3 GIRs`  FIRs `FIR%` `Iron FIRs` `Iron FIR%` `Driver FIRs` `Driver FIR%`
    ##   <date>     <chr>                                      <dbl> <int>  <dbl>        <dbl> <int>  <dbl>       <dbl>       <dbl>         <dbl>         <dbl>
    ## 1 2026-07-03 "2026-07-03\nRandolph North\n10.6"          69.8     5   27.8            1     1    7.1           0       NaN               1           7.1
    ## 2 2026-06-28 "2026-06-28\nSilverbell\n10.4"              68       4   22.2            0     4   30.8           0       NaN               3          25  
    ## 3 2026-06-28 "2026-06-28\nSilverbell\n10.4"              68.9     4   22.2            0     4   30.8           0       NaN               3          25  
    ## 4 2026-06-21 "2026-06-21\nRandolph North\n10.4"          69.8     9   50              1     6   42.9           1       100               5          38.5
    ## 5 2026-06-07 "2026-06-07\nRandolph North\n11"            69.8     6   33.3            1     4   28.6           1       100               3          23.1
    ## 6 2026-05-31 "2026-05-31\nFred Enke\n11"                 68.6     5   27.8            2     3   21.4           1        12.5             2          33.3

#### Club Metrics

Yardage and accuracy for each club

    ## # A tibble: 6 × 6
    ## # Groups:   date [1]
    ##   date       club      n rd_min_yds_to_target rd_min_yds_traveled rd_min_yd_diff
    ##   <date>     <chr> <int>                <dbl>               <dbl>          <dbl>
    ## 1 2026-07-03 3W        1                  250                 100            150
    ## 2 2026-07-03 4         7                   82                  16            -16
    ## 3 2026-07-03 5         4                  106                  85            -23
    ## 4 2026-07-03 6         2                  179                 177             -3
    ## 5 2026-07-03 7         1                  168                 195            -27
    ## 6 2026-07-03 8         2                  168                 169            -10

    ## # A tibble: 6 × 6
    ## # Groups:   date [1]
    ##   date       club      n rd_max_yds_to_target rd_max_yds_traveled rd_max_yd_diff
    ##   <date>     <chr> <int>                <dbl>               <dbl>          <dbl>
    ## 1 2026-07-03 3W        1                  250                 100            150
    ## 2 2026-07-03 4         7                  238                 194            222
    ## 3 2026-07-03 5         4                  210                 219             48
    ## 4 2026-07-03 6         2                  189                 192              2
    ## 5 2026-07-03 7         1                  168                 195            -27
    ## 6 2026-07-03 8         2                  168                 178             -1

    ## # A tibble: 6 × 10
    ## # Groups:   date [1]
    ##   date       club  `rd club strokes` miss_direction `rd club miss dir` rd_avg_yds_to_target rd_avg_yds_traveled rd_avg_yd_diff rd_avg_accuracy `rd club % miss direction`
    ##   <date>     <chr>             <int> <chr>                       <int>                <dbl>               <dbl>          <dbl>           <dbl>                      <dbl>
    ## 1 2026-07-03 3W                    1 short                           1                 250                 100           150                 0                        100
    ## 2 2026-07-03 4                     2 on_target                       2                 198                 190             8               100                        100
    ## 3 2026-07-03 5                     2 long                            2                 194.                210.          -16                 0                        100
    ## 4 2026-07-03 6                     2 long                            1                 184                 184.           -0.5               0                         50
    ## 5 2026-07-03 6                     2 right                           1                 184                 184.           -0.5               0                         50
    ## 6 2026-07-03 7                     1 long                            1                 168                 195           -27                 0                        100

## Plot Metric Summaries

### Scoring Metrics

``` r
scoring_metrics |> 
  dplyr::mutate(course_rating = dplyr::case_when(grepl(date_course, pattern = 'Sewailo') ~ 68.9, TRUE ~ course_rating),
                `Handicap Index` = `Handicap Index`*4.5) |>
  dplyr::rename(`Course Rating` = course_rating) |> 
  dplyr::group_by(date, date_course, `Handicap Index`) |> 
  tidyr::pivot_longer(cols = c(`Course Rating`:`Net Score`), names_to = 'metric', values_to = 'value', values_drop_na = F) |> 
  ggplot2::ggplot(aes(x = date_course, y = value)) +
  ggplot2::geom_point(aes(x = date, y = value, size = 4, alpha = 0.1,
                          color = factor(metric,
                                         levels = c('Handicap Index', 'Course Rating', 'Gross Score', 'Net Score')),
                          fill = factor(metric,
                                         levels = c('Handicap Index', 'Course Rating', 'Gross Score', 'Net Score')))) +
  ggplot2::geom_line(aes(x = date, y = value, size = 1, alpha = 0.1,
                          color = factor(metric,
                                         levels = c('Handicap Index', 'Course Rating', 'Gross Score', 'Net Score')),
                          fill = factor(metric,
                                         levels = c('Handicap Index', 'Course Rating', 'Gross Score', 'Net Score')))) +
  ggplot2::geom_smooth(aes(x = date, y = value, group = metric,
                           color = factor(metric,
                                          levels = c('Handicap Index', 'Course Rating', 'Gross Score', 'Net Score')),
                           fill = factor(metric,
                                         levels = c('Handicap Index', 'Course Rating', 'Gross Score', 'Net Score'))),
                       alpha = 0.3, method = 'lm') +
  ggplot2::scale_y_continuous(sec.axis = ggplot2::sec_axis(~./4.5, name = 'Handicap Index'))+
  ggplot2::labs(title = paste0('Performance Over Time\n',
                               scores |> 
                                 dplyr::distinct(date) |> 
                                 dplyr::last() |> 
                                 unlist() %>% lubridate::as_date(.) |> 
                                 as.character(),
                               ' - ',
                               scores |> 
                                 dplyr::distinct(date) |> 
                                 dplyr::first() |> 
                                 unlist() %>% lubridate::as_date(.) |> 
                                 as.character()),
                x = 'date',
                y = 'metric',
                color = 'metric'
                ) +
  ggplot2::guides(alpha = 'none', size = 'none', fill = 'none') +
  ggplot2::theme_bw() +
  ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
                 axis.title = ggplot2::element_text(face = 'bold'),
                 axis.text.y = ggplot2::element_text(face = 'bold'),
                 axis.text.x = ggplot2::element_text(face = 'bold', angle = 0, hjust = 0, vjust = 0),
                 title = ggplot2::element_text(face = 'bold')
                 ) +
  ggplot2::scale_x_date(date_breaks = '1 month', date_labels = '%b')
```

![](../figures/scorecard_update/PlotScoringMetrics-1.png)<!-- -->

### Stroke Metrics

``` r
stroke_metrics |> 
  tidyr::pivot_longer(cols = c(`doubles+`:birdies), names_to = 'metric', values_to = 'value', values_drop_na = F) |> 
  ggplot2::ggplot(aes(x = date_course, y = value, group = metric, color = metric, fill = metric)) +
  ggplot2::geom_point(aes(x = date, y = value, size = 4, alpha = 0.1), na.rm = T) +
  ggplot2::geom_line(aes(x = date, y = value, size = 1, alpha = 0.1), na.rm = T) +
  ggplot2::geom_smooth(aes(x = date, y = value), alpha = 0.3, method = 'lm') +
  ggplot2::labs(title = paste0('Hole-by-Hole Scores Over Time\n',
                               scores |> 
                                 dplyr::distinct(date) |> 
                                 dplyr::last() |> 
                                 unlist() %>% lubridate::as_date(.) |> 
                                 as.character(),
                               ' - ',
                               scores |> 
                                 dplyr::distinct(date) |> 
                                 dplyr::first() |> 
                                 unlist() %>% lubridate::as_date(.) |> 
                                 as.character()),
                x = 'date',
                y = 'metric',
                color = 'metric', fill = 'metric') +
  ggplot2::guides(alpha = 'none', size = 'none') +
  ggplot2::theme_bw() +
  ggplot2::theme(legend.position = 'none',
                 panel.grid.minor = ggplot2::element_blank(),
                 axis.title = ggplot2::element_text(face = 'bold'),
                 axis.text.y = ggplot2::element_text(face = 'bold'),
                 axis.text.x = ggplot2::element_text(face = 'bold', angle = 270, hjust = 0, vjust = 0.5),
                 title = ggplot2::element_text(face = 'bold'),
                 strip.text.x.top = ggplot2::element_text(face = 'bold'),
                 strip.background = ggplot2::element_rect(color = 'black', 
                                                          fill = 'white')) +
  ggplot2::scale_x_date(date_breaks = '1 month', date_labels = '%b') +
  ggplot2::facet_wrap(~factor(metric, levels = c('doubles+', 'bogies', 'pars', 'birdies')), nrow = 1)
```

![](../figures/scorecard_update/PlotStrokeMetrics-1.png)<!-- -->

### Around the Green Metrics

``` r
atg_metrics |> 
  tidyr::pivot_longer(cols = c(chips:`Avg GIR putts`), names_to = 'metric', values_to = 'value', values_drop_na = F) |> 
  ggplot2::ggplot(aes(x = date_course, y = value, group = metric, color = metric, fill = metric)) +
  ggplot2::geom_point(aes(x = date, y = value, size = 4, alpha = 0.1), na.rm = T) +
  ggplot2::geom_line(aes(x = date, y = value, size = 1, alpha = 0.1), na.rm = T) +
  ggplot2::geom_smooth(aes(x = date, y = value), alpha = 0.3, method = 'lm') +
  ggplot2::labs(title = paste0('Around the Green Metrics Over Time\n',
                               scores |> 
                                 dplyr::distinct(date) |> 
                                 dplyr::last() |> 
                                 unlist() %>% lubridate::as_date(.) |> 
                                 as.character(),
                               ' - ',
                               scores |> 
                                 dplyr::distinct(date) |> 
                                 dplyr::first() |> 
                                 unlist() %>% lubridate::as_date(.) |> 
                                 as.character()),
                x = 'date',
                y = 'metric',
                color = 'metric', fill = 'metric') +
  ggplot2::guides(alpha = 'none', size = 'none') +
  ggplot2::theme_bw() +
  ggplot2::theme(legend.position = 'none',
                 panel.grid.minor = ggplot2::element_blank(),
                 axis.title = ggplot2::element_text(face = 'bold'),
                 axis.text.y = ggplot2::element_text(face = 'bold'),
                 axis.text.x = ggplot2::element_text(face = 'bold', angle = 270, hjust = 0, vjust = 0.5),
                 title = ggplot2::element_text(face = 'bold'),
                 strip.text.x.top = ggplot2::element_text(face = 'bold'),
                 strip.background = ggplot2::element_rect(color = 'black', 
                                                          fill = 'white')) +
  ggplot2::scale_x_date(date_breaks = '1 month', date_labels = '%b') +
  ggplot2::facet_wrap(~factor(metric, levels = c('chips', 'putts', 'chips+putts', 'UpAndDown%', 'Avg GIR putts')), scales = 'free')
```

![](../figures/scorecard_update/PlotAroundTheGreenMetrics-1.png)<!-- -->

### Ball Striking Metrics

``` r
ball_striking_metrics |> 
  tidyr::pivot_longer(cols = c(GIRs:`Driver FIR%`), names_to = 'metric', values_to = 'value', values_drop_na = F) |> 
  ggplot2::ggplot(aes(x = date_course, y = value, group = metric, color = metric, fill = metric)) +
  ggplot2::geom_point(aes(x = date, y = value, size = 4, alpha = 0.1), na.rm = T) +
  ggplot2::geom_line(aes(x = date, y = value, size = 1, alpha = 0.1), na.rm = T) +
  ggplot2::geom_smooth(aes(x = date, y = value), alpha = 0.3, method = 'lm') +
  ggplot2::labs(title = paste0('Ball Striking Over Time\n',
                               scores |> 
                                 dplyr::distinct(date) |> 
                                 dplyr::last() |> 
                                 unlist() %>% lubridate::as_date(.) |> 
                                 as.character(),
                               ' - ',
                               scores |> 
                                 dplyr::distinct(date) |> 
                                 dplyr::first() |> 
                                 unlist() %>% lubridate::as_date(.) |> 
                                 as.character()),
                x = 'date',
                y = 'metric',
                color = 'metric', fill = 'metric') +
  ggplot2::guides(alpha = 'none', size = 'none') +
  ggplot2::theme_bw() +
  ggplot2::theme(legend.position = 'none',
                 panel.grid.minor = ggplot2::element_blank(),
                 axis.title = ggplot2::element_text(face = 'bold'),
                 axis.text.y = ggplot2::element_text(face = 'bold'),
                 axis.text.x = ggplot2::element_text(face = 'bold', angle = 270, hjust = 0, vjust = 0.5),
                 title = ggplot2::element_text(face = 'bold'),
                 strip.text.x.top = ggplot2::element_text(face = 'bold'),
                 strip.background = ggplot2::element_rect(color = 'black', 
                                                          fill = 'white')) +
  ggplot2::scale_x_date(date_breaks = '1 month', date_labels = '%b') +
  ggplot2::facet_wrap(~factor(metric,levels = c('Driver FIRs', 'Iron FIRs', 'FIRs',
                                                'Driver FIR%', 'Iron FIR%', 'FIR%',
                                                'GIRs', 'Par 3 GIRs', 'GIR%')), scales = 'free')
```

![](../figures/scorecard_update/PlotBallStrikingMetrics-1.png)<!-- -->

### Stroke Quality Metrics

#### Minima

``` r
stroke_quality_min |> 
  dplyr::ungroup() %>%
  dplyr::mutate(dplyr::across(c(date:n), ~as.character(.x))) |> 
  tidyr::pivot_longer(cols = c(dplyr::contains('yd')), names_to = 'metric', values_to = 'vals') |> 
  dplyr::mutate(metric = dplyr::case_when(grepl(metric, pattern = 'target') ~ 'min. target distance',
                                          grepl(metric, pattern = 'traveled') ~ 'min. shot distance',
                                          grepl(metric, pattern = 'diff') ~ 'min.\ntarget dist. - shot dist.')) |> 
  dplyr::mutate(date = lubridate::as_date(date),
                club = as.character(club),
                n = as.integer(n)) |> 
  ggplot2::ggplot(aes(x = date, y = vals, group = metric, color = club, fill = club)) +
  ggplot2::geom_point(alpha = 0.4, size = 3) +
  ggplot2::geom_line(alpha = 0.4, size = 2) +
  ggplot2::geom_smooth(method = 'lm', se = F) +
  ggplot2::labs(title = 'Stroke Yardage Minima by Club', x = 'date', y = 'value (yards)', fill = 'club') +
  ggplot2::guides(alpha = 'none', size = 'none') +
  ggplot2::theme_bw() +
  ggplot2::theme(title = ggplot2::element_text(face = 'bold', size = 12),
                 axis.title = ggplot2::element_text(face = 'bold', size = 10),
                 axis.text = ggplot2::element_text(face = 'bold', size = 10),
                 axis.text.x = ggplot2::element_text(face = 'bold', size = 7, angle = 270, hjust = 0, vjust = 0.5),
                 legend.position = 'none',
                 strip.background = ggplot2::element_rect(fill = 'white'),
                 strip.text.y.right = ggplot2::element_text(face = 'bold', size = 9),
                 strip.text.x.top = ggplot2::element_text(face = 'bold', size = 10)) + 
  ggplot2::scale_x_date(date_breaks = 'weeks', date_labels = '%b %d') +
  ggplot2::facet_grid(
    
    rows = vars(metric), 
    
    cols = vars(factor(club, levels = c('D', '3W', '4', '5',
                                        '6', '7', '8', '9',
                                        'PW', 'GW', 'SW', 'LW', 'P'))),
                      scales = 'free')
```

![](../figures/scorecard_update/PlotStrokeQualityMinMetrics-1.png)<!-- -->

#### Maxima

``` r
stroke_quality_max |> 
  dplyr::ungroup() %>%
  dplyr::mutate(dplyr::across(c(date:n), ~as.character(.x))) |> 
  tidyr::pivot_longer(cols = c(dplyr::contains('yd')), names_to = 'metric', values_to = 'vals') |> 
  dplyr::mutate(metric = dplyr::case_when(grepl(metric, pattern = 'target') ~ 'max. target distance',
                                          grepl(metric, pattern = 'traveled') ~ 'max. shot distance',
                                          grepl(metric, pattern = 'diff') ~ 'max.\ntarget dist. - shot dist.')) |> 
  dplyr::mutate(date = lubridate::as_date(date),
                club = as.character(club),
                n = as.integer(n)) |> 
  ggplot2::ggplot(aes(x = date, y = vals, group = metric, color = club, fill = club)) +
  ggplot2::geom_point(alpha = 0.4, size = 3) +
  ggplot2::geom_line(alpha = 0.4, size = 2) +
  ggplot2::geom_smooth(method = 'lm', se = F) +
  ggplot2::labs(title = 'Stroke Yardage Maxima by Club', x = 'date', y = 'value (yards)', fill = 'club') +
  ggplot2::guides(alpha = 'none', size = 'none') +
  ggplot2::theme_bw() +
  ggplot2::theme(title = ggplot2::element_text(face = 'bold', size = 12),
                 axis.title = ggplot2::element_text(face = 'bold', size = 10),
                 axis.text = ggplot2::element_text(face = 'bold', size = 10),
                 axis.text.x = ggplot2::element_text(face = 'bold', size = 7, angle = 270, hjust = 0, vjust = 0.5),
                 legend.position = 'none',
                 strip.background = ggplot2::element_rect(fill = 'white'),
                 strip.text.y.right = ggplot2::element_text(face = 'bold', size = 9),
                 strip.text.x.top = ggplot2::element_text(face = 'bold', size = 10)) + 
  ggplot2::scale_x_date(date_breaks = 'weeks', date_labels = '%b %d') +
  ggplot2::facet_grid(
    
    rows = vars(metric), 
    
    cols = vars(factor(club, levels = c('D', '3W', '4', '5',
                                        '6', '7', '8', '9',
                                        'PW', 'GW', 'SW', 'LW', 'P'))),
                      scales = 'free')
```

![](../figures/scorecard_update/PlotStrokeQualityMaxMetrics-1.png)<!-- -->

#### Full Stroke Average

``` r
full_stroke_quality_avg |> 
  dplyr::ungroup() %>%
  dplyr::mutate(dplyr::across(dplyr::everything(), ~as.character(.x))) |> 
  tidyr::pivot_longer(cols = c(dplyr::contains("rd")), names_to = 'metric', values_to = 'vals') |> 
  dplyr::mutate(metric = dplyr::case_when(grepl(metric, pattern = 'target') ~ 'avg. target distance',
                                          grepl(metric, pattern = 'traveled') ~ 'avg. shot distance',
                                          grepl(metric, pattern = 'diff') ~ 'avg.\ntarget dist. - shot dist.',
                                          # grepl(metric, pattern = 'accuracy') ~ 'avg\naccuracy',
                                          grepl(metric, pattern = 'strokes') ~ 'n strokes',
                                          grepl(metric, pattern = 'dir') ~ 'strokes w/club\nin round\nby direction',
                                          grepl(metric, pattern = 'acc') ~ '% accuracy')) |> 
  dplyr::filter(!grepl(metric, pattern = 'strokes')) |>
  dplyr::mutate(date = lubridate::as_date(date),
                club = as.character(club),
                vals = as.numeric(vals)
                ) |> 
  ggplot2::ggplot(aes(x = date, y = vals, group = metric, color = club, fill = club)) +
  ggplot2::geom_point(alpha = 0.4, size = 3) +
  ggplot2::geom_line(alpha = 0.4, size = 2) +
  ggplot2::geom_smooth(method = 'lm', se = F) +
  ggplot2::labs(title = 'Full Stroke Averages by Club', x = 'date', y = 'value (yards or units)', fill = 'club') +
  ggplot2::guides(alpha = 'none', size = 'none') +
  ggplot2::theme_bw() +
  ggplot2::theme(title = ggplot2::element_text(face = 'bold', size = 12),
                 axis.title = ggplot2::element_text(face = 'bold', size = 10),
                 axis.text = ggplot2::element_text(face = 'bold', size = 10),
                 axis.text.x = ggplot2::element_text(face = 'bold', size = 7, angle = 270, hjust = 0, vjust = 0.5),
                 legend.position = 'none',
                 strip.background = ggplot2::element_rect(fill = 'white'),
                 strip.text.y.right = ggplot2::element_text(face = 'bold', size = 9),
                 strip.text.x.top = ggplot2::element_text(face = 'bold', size = 10)) + 
  ggplot2::scale_x_date(date_breaks = 'weeks', date_labels = '%b %d') +
  ggplot2::facet_grid(
    
    rows = vars(metric), 
    
    cols = vars(factor(club, levels = c('D', '3W', '4', '5',
                                        '6', '7', '8', '9',
                                        'PW', 'GW', 'SW', 'LW', 'P'))),
                      scales = 'free')
```

![](../figures/scorecard_update/PlotStrokeQualityMetricAverages-1.png)<!-- -->

### Main Metrics

``` r
scores_sum |> 
  dplyr::mutate(course_rating = dplyr::case_when(grepl(date_course, pattern = 'Sewailo') ~ 68.9, TRUE ~ course_rating)) |> 
  tidyr::pivot_longer(cols = c(`Handicap Index`, `Gross Score`, `Net Score`,
                               `doubles+`, bogies, pars, birdies, chips, putts,
                               `UpDown%`,`Avg GIR putts`, `GIR%`, `Par 3 GIRs`,
                               `Iron FIR%`, `Driver FIR%`), names_to = 'metric', values_to = 'value', values_drop_na = F) |> 
  ggplot2::ggplot(aes(x = date_course, y = value, group = metric, color = metric, fill = metric)) +
  ggplot2::geom_point(aes(x = date, y = value, size = 4, alpha = 0.1), na.rm = T) +
  ggplot2::geom_line(aes(x = date, y = value, size = 1, alpha = 0.1), na.rm = T) +
  ggplot2::geom_smooth(aes(x = date, y = value), alpha = 0.3, method = 'lm') +
  ggplot2::labs(title = paste0('Performance Over Time\n',
                               scores |> 
                                 dplyr::distinct(date) |> 
                                 dplyr::last() |> 
                                 unlist() %>% lubridate::as_date(.) |> 
                                 as.character(),
                               ' - ',
                               scores |> 
                                 dplyr::distinct(date) |> 
                                 dplyr::first() |> 
                                 unlist() %>% lubridate::as_date(.) |> 
                                 as.character()),
                x = 'date',
                y = 'metric',
                color = 'metric', fill = 'metric') +
  ggplot2::guides(alpha = 'none', size = 'none') +
  ggplot2::theme_bw() +
  ggplot2::theme(legend.position = 'none',
                 panel.grid.minor = ggplot2::element_blank(),
                 axis.title = ggplot2::element_text(face = 'bold'),
                 axis.text.y = ggplot2::element_text(face = 'bold'),
                 axis.text.x = ggplot2::element_text(face = 'bold', angle = 270, hjust = 0, vjust = 0.5),
                 title = ggplot2::element_text(face = 'bold'),
                 strip.text.x.top = ggplot2::element_text(face = 'bold'),
                 strip.background = ggplot2::element_rect(color = 'black',
                                                          fill = 'white')) +
  ggplot2::scale_x_date(date_breaks = '1 month', date_labels = '%b') +
  ggplot2::facet_wrap(~factor(metric,
                      levels = c('Handicap Index', 'Gross Score', 'Net Score',
                                 'doubles+', 'bogies', 'pars', 'birdies',
                                 'chips', 'putts', 'UpDown%', 'Avg GIR putts',
                                 'GIR%', 'Par 3 GIRs', 'Iron FIR%', 'Driver FIR%')),
                      scales = 'free',
                      ncol = 4)
```

![](../figures/scorecard_update/PlotMainMetrics-1.png)<!-- -->
