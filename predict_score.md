## Predicting Golf Gross Score with a LMER Model
====
Erik Larsen

2026-01-22

## Environment

### Attach Packages

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

### Connect to the db

``` r
con <- RSQLite::dbConnect(drv = RSQLite::SQLite(), dbname = 'golf_data')
```

## Summarize Metrics

### Gather and Format

Gather and format from the database


``` r
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

### Compute Advanced Metrics

Compute more nuanced metrics

``` r
head(scores_sum)
```

```
#### ## A tibble: 6 × 25
#### ## Groups:   date, date_course, course_rating [6]
####   date       date_course        course_rating `Handicap Index`  FIRs `Iron FIRs`
####   <date>     <chr>                      <dbl>            <dbl> <dbl>       <dbl>
#### 1 2025-05-04 "2025-05-04\nRand…          69.8             11.3    NA          NA
#### 2 2025-05-18 "2025-05-18\nDell…          67.8             11.3    NA          NA
#### 3 2025-06-01 "2025-06-01\nSilv…          68.9             11.8    NA          NA
#### 4 2025-06-08 "2025-06-08\nDell…          67.8             12.1    NA          NA
#### 5 2025-06-22 "2025-06-22\nRand…          69.8             12.9    NA          NA
#### 6 2025-07-13 "2025-07-13\nRand…          69.8             13.3    10           2
#### ## ℹ 19 more variables: `Iron FIR%` <dbl>, `Driver FIRs` <dbl>,
#### ##   `Driver FIR%` <dbl>, `FIR%` <dbl>, GIRs <dbl>, `Par 3 GIRs` <dbl>,
#### ##   `GIR%` <dbl>, putts <dbl>, `Avg GIR putts` <dbl>, chips <dbl>,
#### ##   `chips+putts` <dbl>, `UpDown%` <dbl>, pars <int>, birdies <int>,
#### ##   bogies <int>, `doubles+` <int>, penalties <dbl>, `Gross Score` <dbl>,
#### ##   `Net Score` <dbl>
```

### Separate Metrics

Separate the metrics:

#### Scoring Metrics

Round scores and `Handicap Index`

``` r
scoring_metrics <- scores_sum |> 
  dplyr::select(`Handicap Index`, `Gross Score`, `Net Score`)
head(scoring_metrics)
```

```
#### ## A tibble: 6 × 6
#### ## Groups:   date, date_course, course_rating [6]
####   date       date_course            course_rating `Handicap Index` `Gross Score`
####   <date>     <chr>                          <dbl>            <dbl>         <dbl>
#### 1 2025-05-04 "2025-05-04\nRandolph…          69.8             11.3            88
#### 2 2025-05-18 "2025-05-18\nDell Uri…          67.8             11.3            90
#### 3 2025-06-01 "2025-06-01\nSilverbe…          68.9             11.8            93
#### 4 2025-06-08 "2025-06-08\nDell Uri…          67.8             12.1            88
#### 5 2025-06-22 "2025-06-22\nRandolph…          69.8             12.9            87
#### 6 2025-07-13 "2025-07-13\nRandolph…          69.8             13.3            84
#### ## ℹ 1 more variable: `Net Score` <dbl>
```

#### Stroke Metrics

Above/below par

``` r
stroke_metrics <- scores_sum |> 
  dplyr::select(`doubles+`, bogies, pars, birdies)
head(stroke_metrics)
```

```
#### ## A tibble: 6 × 7
#### ## Groups:   date, date_course, course_rating [6]
####   date       date_course           course_rating `doubles+` bogies  pars birdies
####   <date>     <chr>                         <dbl>      <int>  <int> <int>   <int>
#### 1 2025-05-04 "2025-05-04\nRandolp…          69.8          3      9     6       0
#### 2 2025-05-18 "2025-05-18\nDell Ur…          67.8          7      6     5       0
#### 3 2025-06-01 "2025-06-01\nSilverb…          68.9          7      8     3       0
#### 4 2025-06-08 "2025-06-08\nDell Ur…          67.8          5      7     5       1
#### 5 2025-06-22 "2025-06-22\nRandolp…          69.8          3      9     6       0
#### 6 2025-07-13 "2025-07-13\nRandolp…          69.8          1     12     3       2
```

#### Around-the-Green Metrics

Chips, putts, etc.

``` r
atg_metrics <- scores_sum |> 
  dplyr::select(chips, `chips+putts`, `UpDown%`, putts, `Avg GIR putts`)
head(atg_metrics)
```

```
#### ## A tibble: 6 × 8
#### ## Groups:   date, date_course, course_rating [6]
####   date       date_course       course_rating chips `chips+putts` `UpDown%` putts
####   <date>     <chr>                     <dbl> <dbl>         <dbl>     <dbl> <dbl>
#### 1 2025-05-04 "2025-05-04\nRan…          69.8    NA            NA        NA    NA
#### 2 2025-05-18 "2025-05-18\nDel…          67.8    NA            NA        NA    NA
#### 3 2025-06-01 "2025-06-01\nSil…          68.9    NA            NA        NA    NA
#### 4 2025-06-08 "2025-06-08\nDel…          67.8    NA            NA        NA    NA
#### 5 2025-06-22 "2025-06-22\nRan…          69.8    NA            NA        NA    NA
#### 6 2025-07-13 "2025-07-13\nRan…          69.8    NA            NA        NA    28
#### ## ℹ 1 more variable: `Avg GIR putts` <dbl>
```

#### Ball Striking

Approach and tee accuracy

``` r
ball_striking_metrics <- scores_sum |> 
  dplyr::select(GIRs, `GIR%`, `Par 3 GIRs`,
                FIRs, `FIR%`, `Iron FIRs`, `Iron FIR%`,
                `Driver FIRs`, `Driver FIR%`)
head(ball_striking_metrics)
```

```
#### ## A tibble: 6 × 12
#### ## Groups:   date, date_course, course_rating [6]
####   date       date_course    course_rating  GIRs `GIR%` `Par 3 GIRs`  FIRs `FIR%`
####   <date>     <chr>                  <dbl> <dbl>  <dbl>        <dbl> <dbl>  <dbl>
#### 1 2025-05-04 "2025-05-04\n…          69.8    NA   NA             NA    NA   NA  
#### 2 2025-05-18 "2025-05-18\n…          67.8    NA   NA             NA    NA   NA  
#### 3 2025-06-01 "2025-06-01\n…          68.9    NA   NA             NA    NA   NA  
#### 4 2025-06-08 "2025-06-08\n…          67.8    NA   NA             NA    NA   NA  
#### 5 2025-06-22 "2025-06-22\n…          69.8    NA   NA             NA    NA   NA  
#### 6 2025-07-13 "2025-07-13\n…          69.8     3   16.7            0    10   71.4
#### ## ℹ 4 more variables: `Iron FIRs` <dbl>, `Iron FIR%` <dbl>,
#### ##   `Driver FIRs` <dbl>, `Driver FIR%` <dbl>
```

## LMER Model

### Fit LMER

Fit a LMER (linear mixed-effects regression) model to the data to capture repeated measurements of `Gross Score` 
predicted by `Handicap Index` and `course_rating`.

+ The `USGA` calculates this index based on an individual's **average of the 8 best** `Gross Score`s over their **20 most recently-posted** rounds.
+ Every course has a rating; the `Handicap Index` calculation factors in these ratings.

+ Center the `course_rating` and `Handicap Index` variables at their mean to make interpreting intercept and slope estimates more meaningful.

+ Include random intercepts and random slopes of `course` and `course_rating`, given
a `Handicap Index`.


``` r
## Fit a model to the data

gross_lmer <- lme4::lmer(
  
  data = scores_sum |> 
    dplyr::ungroup() |> 
    dplyr::mutate(
      
      course_rating = mean(course_rating) - course_rating, 
      
      course = gsub(date_course,
                    pattern = '[0-9]|\\-|\\\n|\\.',
                    replacement = ''), ## extract the course names
      
      `Handicap Index` = mean(`Handicap Index`) - `Handicap Index`,
      days = as.numeric(date - min(date) + 1,
                        units = 'days')
      ) |> ## create a 'days' metric starting at the first day joining the club 
    
    dplyr::relocate(days, .after = date),
  
  formula = 
    `Gross Score` ~
    `Handicap Index` +
    course_rating +
    course + 
    days +
    (1 + `Handicap Index`|course) + ## random intercepts and random slopes for Gross Score at a course given a Handicap Index
    (1 + `Handicap Index`|course_rating) ## random intercepts and random slopes for Gross Score at a course rating given a Handicap Index
           )
```

### LMER Model Summary


```
#### Linear mixed model fit by REML ['lmerMod']
#### Formula: `Gross Score` ~ `Handicap Index` + course_rating + course + days +  
####     (1 + `Handicap Index` | course) + (1 + `Handicap Index` |  
####     course_rating)
####    Data: 
#### dplyr::relocate(dplyr::mutate(dplyr::ungroup(scores_sum), course_rating = mean(course_rating) -  
####     course_rating, course = gsub(date_course, pattern = "[0-9]|\\-|\\\n|\\.",  
####     replacement = ""), `Handicap Index` = mean(`Handicap Index`) -  
####     `Handicap Index`, days = as.numeric(date - min(date) + 1,  
####     units = "days")), days, .after = date)
#### 
#### REML criterion at convergence: 100.9
#### 
#### Scaled residuals: 
####     Min      1Q  Median      3Q     Max 
#### -1.0288 -0.5602 -0.2034  0.3862  2.1646 
#### 
#### Random effects:
####  Groups        Name             Variance  Std.Dev.  Corr 
####  course_rating (Intercept)      5.332e+00 2.3091223      
####                `Handicap Index` 7.885e+00 2.8080372 -1.00
####  course        (Intercept)      8.399e+00 2.8980706      
####                `Handicap Index` 9.317e-07 0.0009653 -1.00
####  Residual                       7.249e+00 2.6924201      
#### Number of obs: 24, groups:  course_rating, 6; course, 5
#### 
#### Fixed effects:
####                      Estimate Std. Error t value
#### (Intercept)          86.43601   16.14408   5.354
#### `Handicap Index`      2.95893    1.83101   1.616
#### course_rating         2.37920    4.94694   0.481
#### courseDell Urich      2.67616   11.21732   0.239
#### courseRandolph North  3.49069   20.26945   0.172
#### courseSewailo        10.95517   16.79479   0.652
#### courseSilverbell      3.94749   15.59537   0.253
#### days                 -0.04018    0.00984  -4.083
#### 
#### Correlation of Fixed Effects:
####             (Intr) `HInd` crs_rt crsDlU crsRnN crsSwl crsSlv
#### `HndcpIndx` -0.393                                          
#### course_rtng -0.954  0.444                                   
#### corsDllUrch -0.939  0.266  0.854                            
#### crsRndlphNr -0.981  0.369  0.957  0.922                     
#### courseSewal -0.923  0.419  0.889  0.874  0.916              
#### corsSlvrbll -0.973  0.346  0.928  0.928  0.964  0.914       
#### days        -0.270 -0.244  0.211  0.207  0.202  0.099  0.185
#### optimizer (nloptwrap) convergence code: 0 (OK)
#### boundary (singular) fit: see help('isSingular')
```


### Model Interpretation

The aggregate average `Gross Score` (**`(Intercept)` `Estimate` of `Fixed effects`**) is **86.44** (yikes, that's bad). 

For every additional `Handicap Index` point larger than the average `Handicap Index`, `Gross Score` increases by **2.96** strokes.

+ This makes sense: `Handicap Index` is used to compare players of various skill by how many strokes they average. 

  + In other words, a player with great skill will have a low `Handicap Index` (i.e. **0**), meaning they average par for an entire round.
  
  + A worse player will have a higher `Handicap Index`, and in competitions, roughly this amount is subtracted from their score. 
  
    + This effectively quantifies who performed better that day after correcting for skill level. 
    

While this makes sense, I wonder whether I should expect `Handicap Index` to have a larger `Fixed effect` `Estimate`.

  + The effect is significant (**`t value` = ** **1.62**; significance : abs(**t value**) > 1). 
  
  + But `Handicap Index` is a metric directly derived from `Gross Score`, thus, I'm unsure how many strokes (`Gross Score`) index points should be worth. 1? More? Does it vary by skill? Is it uniform?

For every additional `course_rating` point (stroke) greater than the average `course_rating` (~69-70 strokes in this dataset), `Gross Score` increases by **2.38** strokes.

  + This also makes sense: harder courses yield higher `Gross Score`s.
  
    + These courses vary in their difficulty, independent of player skill (`Handicap Index`), by **5.33** strokes on average, even though `course_rating` is supposed to account for course difficulty across all courses. This is the **`Random effects` `Variance` (`Intercept`)**.
    
    + When compared to the `Residual` `Variance`, **7.25**, a `course_rating` variance of **5.33** is very high-- I play differently according to `course_rating`.

The `course` also has an effect on `Gross Score`, with a large variance of **8.4**: I play more consistently at some courses than others.

For every additional `day` in time, `Gross Score` drops by **-0.04** strokes. This is seemingly tiny on a day-to-day basis, but extrapolating to months or weeks, this becomes very evident (**-1.2** strokes per month; **-14.6** strokes per year).

  + Linear extrapolation in this sense is misleading-- there will be variation and probably a limit to lowering `Gross Score`.

  + But this effect is strongly significant (**t value = ** **-4.08**) and appears to be the primary driver of the trend.

### Predict Next Round

Predict the next round's `Gross Score` according to the model.

``` r
#### show the model-predicted gross score for the upcoming round, rounded to the nearest stroke
stats::predict(object = gross_lmer, newdata = new_df, allow.new.levels = T) |>
  as.numeric() %>%
  round(., 0)
```

```
#### [1] 80
```

## Plot Model

### Model

![](predict_score_files/figure-html/PlotModelByCourse-1.png)<!-- -->

The model is a **random intercept**, **random slope** linear mixed-effects regression (LMER) model.

In this case, that means `Gross Score` varies for each course at a given `Handicap Index` in its deviation from the overall mean `Gross Score` (navy blue line) over time: `Silverbell`, `Randolph North`, and `Dell Urich` have their own average `Gross Scores` (intercepts) and slopes (change in `Gross Score` over time)-- notice how the line for each course has a different slope, starting at a different y-intercept

+ The `blue` line is the model's overall fit of the `Gross Score`, accounting for `course`, `course_rating`, `Handicap Index`, and `days` (time).
+ `Silverbell`'s line represents the relationship between `Gross Score`, `course_rating`, `Handicap Index`, and `days` (date/time) at `Silverbell`.
+ `Randolph`'s line represents the relationship between `Gross Score`, `course_rating`, `Handicap Index`, and `days` (date/time) at `Randolph North`.
+ `Dell Urich`'s line represents the relationship between `Gross Score`, `course_rating`, `Handicap Index`, and `days` (date/time) at `Dell Urich`.

### Model Predictions

![](predict_score_files/figure-html/PlotModels-1.png)<!-- -->



### Actual Gross Score vs Predicted Gross Score

![](predict_score_files/figure-html/PlotActualVsPredictedGross-1.png)<!-- -->

This plot of residuals reveals the actual `Gross Score` relative to the `Predicted Gross Score` over time, color-coded by `course`, and annotated by `Handicap Index` at the time of the round.

+ the navy blue line represents the dividing line between over/under performing where:

  + any scores below the line represent rounds where I performed better than the model's prediction.
  + any scores above the line represent rounds where I performed worse than the model's prediction.
  

+ `Randolph North`, `Silverbell`, and `Dell Urich` each have lines representing the trend of actual `Gross Score`s compared to predicted `Gross Score`s at each respective course.

  + I more often score better/lower at `Randolph North` than the model predicts, though, on average, these are closest to the model's predictions.
  
    + This might reveal that these rounds are contributing more weight to the model if they are not simply more reflective of the overall trend.
    
    + This could also reveal that I score more consistently at this course than others.
    
    + This could also mean that, given the downward trend, the course is easier than ratings suggest.
    
  + At `Silverbell` and `Randolph North`, I have been outperforming the model and scoring better over time.
    
  + The variability at `Dell Urich` is substantial, with one large outlier overperformance (~ **-15**), and one moderate outlier underperformance (~ **+6**) re-shaping the slope in the opposite direction: I have been getting worse at `Dell Urich` over time.
  
    + Even with more subsequent sample, this could reveal that I struggle to shoot low `Gross Score`s at `Dell Urich`, despite its easier course rating, and any effect of time.
    
    + Other latent variables may contribute to this variability, such as course/weather/event conditions.

### Actual Net Score vs Predicted Gross Score

![](predict_score_files/figure-html/PlotActualNetVsPredictedGross-1.png)<!-- -->

This plot of residuals shifts the previous plot upward, inverts it about the x-axis, and rotates it slightly about the origin, revealing the actual `Net Score` relative to the `Predicted Gross Score` over time, color-coded by `course`, and annotated by `Handicap Index` at the time of the round.

`Net Score` is roughly `Gross Score` - `Handicap Index`.

+ the navy blue line represents the dividing line between over/under performing where:

  + any scores below the line represent rounds where I performed worse than the model's prediction.
  + any scores above the line represent rounds where I performed better than the model's prediction.
  
`Randolph North`, `Silverbell`, and `Dell Urich` each have lines representing the trend of actual `Net Score`s compared to `Predicted Gross Score`s at each respective course.

  + I more often score better/lower at `Randolph North` than the model predicts, particularly over time, given my handicap; however, on average, these are closest to the model's predictions than other courses.
  
    + This reveals that these rounds could be giving more weight to the model.
    
    + This could also reveal that I more consistently, if slightly, outscore the model at this course, even given my `Handicap Index`.
    
    + This could also mean that the course is easier than ratings suggest.
    
    + My instinct is that my `Handicap Index` was overestimated early on in this time series; it was high, and I frequently played `Randolph North` around then, and shot lower scores, directing the trend downward.
    
  + The variability at `Dell Urich` is substantial, with one large outlier overperformance (~ **-15**), and one moderate outlier underperformance (~ **+6**) re-shaping the slope in the opposite direction (positive as opposed to a negative slope, like `Silverbell` and `Randolph North`).
  
    + Even with more subsequent sample, this could reveal that I struggle to shoot low `Gross Score`s at `Dell Urich`, despite its easier course rating.
    
      + See the plot below for more insight.
    
    + Other latent variables may contribute to this variability, such as course/weather/event conditions.

### Actual Gross Score vs Course Rating

![](predict_score_files/figure-html/PlotGrossScoreVsCourseRating-1.png)<!-- -->


This definitely shows that I struggle at `Dell Urich`-- independent of time, my `Gross Score`s at `Dell Urich` are roughly similar to other courses despite its easier rating-- this would be even more evident without the substantial `Gross Score` **72** outlier.

+ Interestingly, I have scored better at longer/more difficult tees at multiple courses.

+ Removing the effect of time/improved skill, and the wildly underrated `Arizona National` rating, this would otherwise capture the general trend and logic that **higher `course ratings` correlate to higher `Gross Score`s**.

### Actual Gross Score vs Handicap Index

![](predict_score_files/figure-html/PlotGrossScoreVsHandicapIndex-1.png)<!-- -->

This also supports the ideas that, independent of time and `Handicap Index`, I struggle at `Dell Urich` because of the high `Gross Score`s at low `Handicap Index`:

+ Removing the outlier at a **`Handicap Index` of 14**, the `Dell Urich` trend still doesn't reverse, though the overall trend does-- independent of time and one outlier/corrective round, I perform worse at a course with a lower `Handicap Index`.

### Actual Gross Score vs Handicap Index, 72 removed

![](predict_score_files/figure-html/PlotGrossScoreVsHandicapIndexWithout72-1.png)<!-- -->


### Actual Net Score vs Course Rating

![](predict_score_files/figure-html/PlotNetScoreVsCourseDifficulty-1.png)<!-- -->

### Actual Net Score vs Course Rating without 72 and Combo Tees

![](predict_score_files/figure-html/PlotNetScoreVsCourseRatingWithout72AndCombos-1.png)<!-- -->



