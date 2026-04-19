### Overview

This vignette shows the linear mixed model used to measure my
performance for each golf round and use that information to predict my
performance for the next golf round

It does this by incorporating my skill level and where I’ve played in
the past

Skill level is officially determined by the **USGA** as a
`Handicap Index`: how many shots, on average, a player takes to complete
a round at a given course with a given difficulty relative to that
course’s average

- i.e. a player with a **10.0** `Handicap Index` is expected, on
  average, to take **82 shots** to complete a round at a course where
  the average number of strokes has been determined to be **72**

`Gross Score` is a total number of shots hit by a given player for any
individual hole

- this is extrapolated across each of 18 holes, so `Gross Score` can
  mean a total of shots for a hole *or* a total of shots for a round

- the average `Gross Score` over a player’s best **8 rounds** *from
  their last 20* is used to determine their `Handicap Index`

Thus, this vignette shows the model used to predict the opposite:

- given my `Handicap Index` and `Gross Score`s at previous courses of
  varying difficulty in the past,

  - **what will be my next** `Gross Score`?

Note that I use the terms, `strokes`, and, `shots`, interchangeably,
though putts are not connoted as `shots`

### Code Environment Details

#### Attach Packages

``` r
library(golf)
library(tidyverse)
library(lme4)
library(mgcv)
library(brms)
library(DBI)
library(RSQLite)
library(emayili)
```

### Code for Summarizing Metrics

#### Gather and Format Scores

Gather and format from the database

``` r
## connect to the db
con <- golf::get_db_connection()

scores <- DBI::dbGetQuery(conn = con, statement = paste0(
  "SELECT DISTINCT sub.* FROM
  (SELECT DISTINCT r.*, c.par, c.course_rating FROM rounds r
  LEFT JOIN courses c
  ON c.tees = r.tees
  AND c.course_name = r.course_name
  AND c.hole = r.hole
  AND c.hole_handicap = r.hole_handicap) AS sub
  INNER JOIN players p
  ON sub.GHIN = p.GHIN
  AND sub.handicap_index = p.handicap_index
  AND sub.date = p.date;"
)) |> 
  dplyr::mutate(date = lubridate::as_date(date),
                hole = stringr::str_extract(hole, pattern = '[0-9]{1,}') |> 
                  as.numeric()) |> 
  dplyr::relocate(par, .after = hole) |> 
  dplyr::relocate(course_rating, .after = tees) |>
  dplyr::group_by(date) |> 
  dplyr::arrange(desc(date), hole) |> 
  dplyr::ungroup()
```

### Code for Computing Advanced Metrics

#### Compute Metrics

Compute more nuanced metrics

### Advanced Metrics Snapshot

``` r
head(scores_sum |> 
       dplyr::arrange(desc(date)))
```

    ## # A tibble: 6 × 25
    ## # Groups:   date, date_course, course_rating [6]
    ##   date       date_course                        course_rating `Handicap Index`  FIRs `Iron FIRs` `Iron FIR%` `Driver FIRs` `Driver FIR%` `FIR%`  GIRs `Par 3 GIRs` `GIR%` putts `Avg GIR putts` chips `chips+putts` `UpDown%`  pars birdies bogies `doubles+` penalties `Gross Score` `Net Score`
    ##   <date>     <chr>                                      <dbl>            <dbl> <int>       <dbl>       <dbl>         <dbl>         <dbl>  <dbl> <int>        <dbl>  <dbl> <int>           <dbl> <dbl>         <dbl>     <dbl> <int>   <int>  <int>      <int>     <int>         <dbl>       <dbl>
    ## 1 2026-04-05 "2026-04-05\nRandolph North\n11"            71.7             11       6           0         0               6          46.2   42.9     3            1   16.7    31            2       21            52      20       6       0      9          3         2            88          80
    ## 2 2026-03-29 "2026-03-29\nDell Urich\n10"                70.3             10       5           2        66.7             3          30     38.5     7            1   38.9    31            1.71    15            46      20       7       2      5          4         1            82          74
    ## 3 2026-03-08 "2026-03-08\nRandolph North\n10"            71.7             10       2           0         0               2          16.7   14.3     7            2   38.9    33            2.14    12            45      30       9       0      6          3         1            84          75
    ## 4 2026-02-22 "2026-02-22\nDell Urich\n10.1"              68               10.1     7           1        33.3             6          60     53.8     3            1   16.7    34            2       18            52      16.7     5       0      7          6         0            92          82
    ## 5 2026-02-08 "2026-02-08\nRandolph North\n10.2"          70               10.2     2           0         0               2          16.7   14.3     7            3   38.9    35            2.14    11            46      22.2     7       1      8          2         0            83          73
    ## 6 2026-01-25 "2026-01-25\nRandolph North\n10.2"          70               10.2     4           0       NaN               4          28.6   28.6     6            2   33.3    36            2.17    15            51      18.2     7       0      9          2         0            85          75

### Group Metrics

Separate the metrics:

#### Scoring Metrics

Round scores and `Handicap Index`

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
    ## 1 2026-04-05 "2026-04-05\nRandolph North\n11"            71.7             11              88          80
    ## 2 2026-03-29 "2026-03-29\nDell Urich\n10"                70.3             10              82          74
    ## 3 2026-03-08 "2026-03-08\nRandolph North\n10"            71.7             10              84          75
    ## 4 2026-02-22 "2026-02-22\nDell Urich\n10.1"              68               10.1            92          82
    ## 5 2026-02-08 "2026-02-08\nRandolph North\n10.2"          70               10.2            83          73
    ## 6 2026-01-25 "2026-01-25\nRandolph North\n10.2"          70               10.2            85          75

#### Stroke Metrics

Above/below par

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
    ## 1 2026-04-05 "2026-04-05\nRandolph North\n11"            71.7          3      9     6       0
    ## 2 2026-03-29 "2026-03-29\nDell Urich\n10"                70.3          4      5     7       2
    ## 3 2026-03-08 "2026-03-08\nRandolph North\n10"            71.7          3      6     9       0
    ## 4 2026-02-22 "2026-02-22\nDell Urich\n10.1"              68            6      7     5       0
    ## 5 2026-02-08 "2026-02-08\nRandolph North\n10.2"          70            2      8     7       1
    ## 6 2026-01-25 "2026-01-25\nRandolph North\n10.2"          70            2      9     7       0

#### Around-the-Green Metrics

Chips, putts, etc.

``` r
atg_metrics <- scores_sum |> 
  dplyr::select(chips, `chips+putts`, `UpDown%`, putts, `Avg GIR putts`)
head(atg_metrics |> 
       dplyr::arrange(desc(date)))
```

    ## # A tibble: 6 × 8
    ## # Groups:   date, date_course, course_rating [6]
    ##   date       date_course                        course_rating chips `chips+putts` `UpDown%` putts `Avg GIR putts`
    ##   <date>     <chr>                                      <dbl> <dbl>         <dbl>     <dbl> <int>           <dbl>
    ## 1 2026-04-05 "2026-04-05\nRandolph North\n11"            71.7    21            52      20      31            2   
    ## 2 2026-03-29 "2026-03-29\nDell Urich\n10"                70.3    15            46      20      31            1.71
    ## 3 2026-03-08 "2026-03-08\nRandolph North\n10"            71.7    12            45      30      33            2.14
    ## 4 2026-02-22 "2026-02-22\nDell Urich\n10.1"              68      18            52      16.7    34            2   
    ## 5 2026-02-08 "2026-02-08\nRandolph North\n10.2"          70      11            46      22.2    35            2.14
    ## 6 2026-01-25 "2026-01-25\nRandolph North\n10.2"          70      15            51      18.2    36            2.17

#### Ball Striking

Approach and tee accuracy

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
    ## 1 2026-04-05 "2026-04-05\nRandolph North\n11"            71.7     3   16.7            1     6   42.9           0         0               6          46.2
    ## 2 2026-03-29 "2026-03-29\nDell Urich\n10"                70.3     7   38.9            1     5   38.5           2        66.7             3          30  
    ## 3 2026-03-08 "2026-03-08\nRandolph North\n10"            71.7     7   38.9            2     2   14.3           0         0               2          16.7
    ## 4 2026-02-22 "2026-02-22\nDell Urich\n10.1"              68       3   16.7            1     7   53.8           1        33.3             6          60  
    ## 5 2026-02-08 "2026-02-08\nRandolph North\n10.2"          70       7   38.9            3     2   14.3           0         0               2          16.7
    ## 6 2026-01-25 "2026-01-25\nRandolph North\n10.2"          70       6   33.3            2     4   28.6           0       NaN               4          28.6

#### Shot Quality

Yardage and accuracy on tracked shots

``` r
## connect to the db
con <- golf::get_db_connection()

stroke_quality <- DBI::dbGetQuery(conn = con,
                                  statement = paste0("SELECT DISTINCT * FROM club_metrics;")) |>
  dplyr::mutate(date = lubridate::as_date(date),
                hole = gsub(hole, pattern = 'hole_', replacement = ''),
                hole = as.integer(hole)) |> 
  dplyr::mutate(dplyr::across(dplyr::contains("yds"), ~as.integer(.x))) |> 
  dplyr::rename(course = course_name) |> 
  dplyr::group_by(date, hole, stroke) |> 
  dplyr::arrange(date, hole, stroke)
head(stroke_quality |> 
       dplyr::arrange(desc(date)))
```

    ## # A tibble: 6 × 14
    ## # Groups:   date, hole, stroke [6]
    ##   course         date       tees   hole   par gross stroke lie     club  yds_to_target yds_traveled on_target miss_direction shot_type
    ##   <chr>          <date>     <chr> <int> <int> <int>  <int> <chr>   <chr>         <int>        <int> <chr>     <chr>          <chr>    
    ## 1 Randolph North 2026-04-05 blue      1     4     4      1 tee     4               220          219 no        right          tee      
    ## 2 Randolph North 2026-04-05 blue      1     4     4      2 rough   PW              140          131 no        right          full     
    ## 3 Randolph North 2026-04-05 blue      1     4     4      3 fairway PW               10            8 yes       on_target      chip     
    ## 4 Randolph North 2026-04-05 blue      2     4     6      1 tee     D               270          308 no        left           tee      
    ## 5 Randolph North 2026-04-05 blue      2     4     6      2 rough   4               115           82 no        short          punch    
    ## 6 Randolph North 2026-04-05 blue      2     4     6      3 sand    SW               30           16 no        short          gsbunker

### LMER Model

#### Fit a LMER Model

Fit a lmer model to capture repeated measurements of `Gross Score`
predicted by `Handicap Index`, `course_rating`, and time (`days`).

- Every course has a rating; the `Handicap Index` calculation factors in
  these ratings

- Center the `course_rating` and `Handicap Index` variables at their
  mean to make interpreting intercept and slope estimates more
  meaningful

- Include random intercepts and random slopes of `course` and
  `course_rating`, given a `Handicap Index`

``` r
# Fit a model to the data

gross_lmer <- lme4::lmer(
  
  data = scores_sum |> 
    dplyr::ungroup() |> 
    dplyr::mutate(
      
      course_rating = mean(course_rating) - course_rating, 
      
      course = gsub(date_course,
                    pattern = '[0-9]|\\-|\\\n|\\.',
                    replacement = ''), # extract the course names
      
      `Handicap Index` = mean(`Handicap Index`) - `Handicap Index`,
      days = as.numeric(date - min(date) + 1,
                        units = 'days')
      ) |> # create a 'days' metric starting at the first day joining the club 
    
    dplyr::relocate(days, .after = date),
  
  formula = 
    `Gross Score` ~
    `Handicap Index` +
    course_rating +
    course + 
    days +
    (1 + `Handicap Index`|course) + # random intercepts and random slopes for Gross Score at a course given a Handicap Index
    (1 + `Handicap Index`|course_rating) # random intercepts and random slopes for Gross Score at a course rating given a Handicap Index
           )
```

#### LMER Model Summary

    ## Linear mixed model fit by REML ['lmerMod']
    ## Formula: `Gross Score` ~ `Handicap Index` + course_rating + course + days +      (1 + `Handicap Index` | course) + (1 + `Handicap Index` |      course_rating)
    ##    Data: dplyr::relocate(dplyr::mutate(dplyr::ungroup(scores_sum), course_rating = mean(course_rating) -      course_rating, course = gsub(date_course, pattern = "[0-9]|\\-|\\\n|\\.",      replacement = ""), `Handicap Index` = mean(`Handicap Index`) -  
    ##     `Handicap Index`, days = as.numeric(date - min(date) + 1,      units = "days")), days, .after = date)
    ## 
    ## REML criterion at convergence: 143.5
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -1.3923 -0.6164 -0.2016  0.5644  1.8382 
    ## 
    ## Random effects:
    ##  Groups        Name             Variance Std.Dev. Corr 
    ##  course_rating (Intercept)      21.4784  4.6345        
    ##                `Handicap Index`  0.9569  0.9782   -1.00
    ##  course        (Intercept)      17.7971  4.2187        
    ##                `Handicap Index` 12.0015  3.4643   0.09 
    ##  Residual                        9.0663  3.0110        
    ## Number of obs: 30, groups:  course_rating, 7; course, 5
    ## 
    ## Fixed effects:
    ##                       Estimate Std. Error t value
    ## (Intercept)          93.654681   9.349804  10.017
    ## `Handicap Index`      2.535617   2.278252   1.113
    ## course_rating         0.081533   1.462834   0.056
    ## courseDell Urich     -4.945540  10.273306  -0.481
    ## courseRandolph North -0.007814  11.700073  -0.001
    ## courseSewailo         2.510066  14.641846   0.171
    ## courseSilverbell     -3.787574  10.502635  -0.361
    ## days                 -0.034983   0.010627  -3.292
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) `HInd` crs_rt crsDlU crsRnN crsSwl crsSlv
    ## `HndcpIndx`  0.181                                          
    ## course_rtng -0.606 -0.106                                   
    ## corsDllUrch -0.826 -0.162  0.454                            
    ## crsRndlphNr -0.866 -0.143  0.635  0.748                     
    ## courseSewal -0.558  0.239  0.331  0.526  0.494              
    ## corsSlvrbll -0.834 -0.171  0.468  0.773  0.739  0.578       
    ## days        -0.342 -0.217  0.350  0.078  0.146  0.095  0.168
    ## optimizer (nloptwrap) convergence code: 0 (OK)
    ## boundary (singular) fit: see help('isSingular')

#### Export Model Results to db

``` r
golf::export_lm_round_predictions(
  model = gross_lmer,
  scores_sum = scores_sum
)
```

### Model Interpretations

#### Big Picture: Handicap Index as a Fixed Effect Predicting Gross Score

The model’s aggregate average `Gross Score` (**`(Intercept)` `Estimate`
of `Fixed effects`**) is **93.65**. This is model’s estimated average
first `Gross Score`.

My average `Gross Score`, however, is **85.2**.

For every additional `Handicap Index` point larger than the average
`Handicap Index`, my `Gross Score` increases by **2.54** strokes.

- This makes sense because `Gross Score` is directly used to determine
  `Handicap Index` and is positively correlated (high `Gross Score` =
  high `Handicap Index`)

  - In other words, a player with great skill will have a low
    `Handicap Index` (i.e. **0**), meaning they average `par` (the
    course average) for an entire round, while a worse player (who
    averages above `par`, the course average) will have a higher
    `Handicap Index`

    - We know this to be true

    - `Handicap Index` effectively corrects for skill-level to determine
      who performed better that day

- While this makes sense, I wonder whether `Handicap Index` should have
  a larger `Fixed effect` `Estimate`, which essentially asks, “how much
  does this variable predict the target (`Gross Score`, here)?”

  - The effect is significant (**`t value` = ** **1.11**; significance :
    abs(**t value**) \> 1)

  - Again, `Handicap Index` is a metric *directly derived from*
    `Gross Score`, thus, I’m unsure how many strokes (`Gross Score`)
    index points should be worth! **1?** **More?** Does it vary by
    skill, or is it uniform?

#### A Deeper Dig: Course, Course Rating, and Time

For every additional `course_rating` point (aka, a stroke) greater than
the average `course_rating` (~69-70 strokes in this dataset),
`Gross Score` increases by **0.08** strokes.

- This also makes sense: harder courses should yield higher
  `Gross Score`s

  - These courses vary in their difficulty, independent of player skill
    (`Handicap Index`), by **21.48** strokes, on average, even though
    `course_rating` is supposed to account for course difficulty across
    all courses. This is the **`Random effects` `Variance`
    (`Intercept`)**.

  - When compared to the `Residual` `Variance`, **9.07**, a
    `course_rating` variance of **21.48** is very high– I play
    differently according to `course_rating`

The `course` also has an effect on `Gross Score`: I play more
consistently at some courses than others, as suggested by the large
variance of **17.8** strokes

For every additional `day` in time, my `Gross Score` drops by **-0.03**
strokes.

- While this seems tiny, extrapolating days to months or weeks, this
  becomes very evident (**-0.9** strokes per month; **-10.95** strokes
  per year)

- Linear extrapolation in this sense is misleading: there will be a
  limit to lowering `Gross Score` and there will also be variation in
  the process

- But this effect is strongly significant (**t value = ** **-3.29**) and
  appears to be the primary driver of the trend

### Predict the Next Round

Predict the next round’s `Gross Score` according to the model

#### Show Prediction

``` r
## show the model-predicted gross score for the upcoming round, rounded to the nearest stroke
stats::predict(object = gross_lmer, newdata = new_df, allow.new.levels = T) |>
  as.numeric() %>%
  round(., 0)
```

    ## [1] 81

### Plot the Model

#### Model by Course

![](../figures/LMER_predictions/PlotModelByCourse-1.png)<!-- -->

The model is a random intercept, random slope linear mixed-effects
regression (LMER) model.

In this case, that means `Gross Score` varies for each course at a given
`Handicap Index` in its deviation from the overall mean `Gross Score`
(navy blue line) over time: `Silverbell`, `Randolph North`, and
`Dell Urich` have their own average `Gross Scores` (intercepts) and
slopes (change in `Gross Score` over time)– notice how the line for each
course has a different slope, starting at a different y-intercept

- The `blue` line is the model’s overall fit of the `Gross Score`,
  accounting for `course`, `course_rating`, `Handicap Index`, and `days`
  (time)
- `Silverbell`’s line represents the relationship between `Gross Score`,
  `course_rating`, `Handicap Index`, and `days` (date/time) at
  `Silverbell`
- `Randolph`’s line represents the relationship between `Gross Score`,
  `course_rating`, `Handicap Index`, and `days` (date/time) at
  `Randolph North`
- `Dell Urich`’s line represents the relationship between `Gross Score`,
  `course_rating`, `Handicap Index`, and `days` (date/time) at
  `Dell Urich`

#### Model Predictions

These predictions are **not** historical– they are current:

- i.e. the model did *not* predict anything close to the **72** in
  August 2025

![](../figures/LMER_predictions/PlotModels-1.png)<!-- -->

#### Actual Gross vs Predicted Gross (performance vs prediction)

![](../figures/LMER_predictions/PlotActualVsPredictedGross-1.png)<!-- -->

This plot of residuals reveals the `Actual Gross Score` relative to the
`Predicted Gross Score` over time, color-coded by `course`, and
annotated by `Handicap Index` at the time of the round.

- the navy blue line represents the dividing line between over/under
  performing where:

  - scores **above** the line = I performed **worse** than the model’s
    prediction
  - scores **below** the line = I performed **better** than the model’s
    prediction

- `Randolph North`, `Silverbell`, and `Dell Urich` each have lines
  representing the trend of actual `Gross Score`s compared to predicted
  `Gross Score`s at each respective course

  - I more often score better/lower at `Randolph North` than the model
    predicts, though, on average, these are closest to the model’s
    predictions

    - This might reveal that these rounds are contributing more weight
      to the model if they are not simply more reflective of the overall
      trend

    - This could also reveal that I score more consistently at this
      course than others

    - This could also mean that, given the downward trend, the course is
      easier than ratings suggest

  - At `Silverbell` and `Randolph North`, I have been outperforming the
    model and scoring better over time

  - The variability at `Dell Urich` is substantial, with one large
    outlier overperformance (~ **-15**), and one moderate outlier
    underperformance (~ **+6**) re-shaping the slope in the opposite
    direction: I have been getting worse at `Dell Urich` over time

    - Even with more subsequent sample, this could reveal that I
      struggle to shoot low `Gross Score`s at `Dell Urich`, despite its
      easier course rating, and any effect of time

    - Other latent variables may contribute to this variability, such as
      course/weather/event conditions

#### Actual Net vs Predicted Gross (skill-adjusted performance vs prediction)

![](../figures/LMER_predictions/PlotActualNetVsPredictedGross-1.png)<!-- -->

Relative to the previous plot, this plot of residuals:

- shifts the previous plot upward

- inverts it about the x-axis

- rotates it slightly about the origin

It shows the `Actual Net Score` relative to the `Predicted Gross Score`
over time, color-coded by `course`, and annotated by `Handicap Index` at
the time of the round.

`Net Score` is roughly `Gross Score` - `Handicap Index`.

- the navy blue line represents the dividing line between over/under
  performing where:

  - scores **below** the line = I performed **worse** than the model’s
    prediction
  - scores **above** the line = I performed **better** than the model’s
    prediction

- `Randolph North`, `Silverbell`, and `Dell Urich` each have lines
  representing the trend of actual `Net Score`s compared to
  `Predicted Gross Score`s at each respective course

  - I more often score better/lower at `Randolph North` than the model
    predicts, particularly over time, given my handicap; however, on
    average, these are closest to the model’s predictions than other
    courses

    - This reveals that these rounds could be giving more weight to the
      model

    - This could also reveal that I more consistently, if slightly,
      outscore the model at this course, even given my `Handicap Index`

    - This could also mean that the course is easier than ratings
      suggest

    - My instinct is that my `Handicap Index` was overestimated early on
      in this time series; it was high, and I frequently played
      `Randolph North` around then, and shot lower scores, directing the
      trend downward

  - The variability at `Dell Urich` is substantial, with one large
    outlier overperformance (~ **-15**), and one moderate outlier
    underperformance (~ **+6**) re-shaping the slope in the opposite
    direction (positive as opposed to a negative slope, like
    `Silverbell` and `Randolph North`)

    - Even with more subsequent sample, this could reveal that I
      struggle to shoot low `Gross Score`s at `Dell Urich`, despite its
      easier course rating

      - See the plot below for more insight

    - Other latent variables may contribute to this variability, such as
      course/weather/event conditions

#### Actual Gross vs Course Rating (performance vs course difficulty)

![](../figures/LMER_predictions/PlotGrossScoreVsCourseRating-1.png)<!-- -->

This definitely shows that I struggle at `Dell Urich`– independent of
time, my `Gross Score`s at `Dell Urich` are roughly similar to other
courses despite its easier rating– this would be even more evident
without the substantial `Gross Score` **72** outlier.

- Interestingly, I have scored better at longer/more difficult tees at
  multiple courses.

- Removing the effect of time/improved skill, and the wildly underrated
  `Arizona National` rating, this would otherwise capture the general
  trend and logic that **higher `course ratings` correlate to higher
  `Gross Score`s**

#### Actual Gross vs Handicap Index (performance vs skill-level)

![](../figures/LMER_predictions/PlotGrossScoreVsHandicapIndex-1.png)<!-- -->

This also supports the ideas that, independent of time and
`Handicap Index`, I struggle at `Dell Urich` because of the high
`Gross Score`s at low `Handicap Index`:

- Removing the outlier at a **`Handicap Index` of 14**, the `Dell Urich`
  trend still doesn’t reverse, though the overall trend does–
  independent of time and one outlier/corrective round, I perform worse
  at a course with a lower `Handicap Index`.

#### Actual Net vs Course Rating (skill-adjusted performance vs course difficulty)

![](../figures/LMER_predictions/PlotNetScoreVsCourseDifficulty-1.png)<!-- -->
