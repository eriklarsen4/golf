#' Run LM + KF modeling pipeline
#'
#' @description
#' Reads production base tables, constructs scores_sum, fits LM + KF,
#' appends to predictions_round, overwrites dev modeling tables,
#' and logs the run. No dev→prod matriculation. No helper functions.
#' 
#' @param db_path optional path to a DuckDB database file; if supplied, the 
#' function uses this database instead of the package's default. Designed for
#' testing with temp writable databases
#' 
#' @return Invisibly returns predictions_round tibble on success.
#' 
#' @import DBI
#' @import dplyr
#' @import lubridate
#' @import lme4
#' @importFrom stats predict var lm
#' @import KFAS
#' @importFrom tibble tibble
#' @export
run_skill_pipeline <- function(db_path = NULL) {
  
  con <- golf::get_db_connection(db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  
  run_started <- Sys.time()
  status <- "success"
  err_msg <- NA_character_
  predictions_round <- NULL
  lm_rows <- NA_integer_
  kf_rows <- NA_integer_
  prod_pred_before <- NA_integer_
  prod_pred_after <- NA_integer_
  
  pipeline_error <- FALSE
  pipeline_error_msg <- NA_character_
  
  # pipeline process to append new predictions to production tables; dev tables ----
  print("BEFORE TRY BLOCK")
  {
    tmp <- try({
      
      print("TRY BLOCK START")
      # read production hole-level data joined to course + player info
      scores <- DBI::dbGetQuery(
        con,
        "
        SELECT DISTINCT r.*, c.par, c.course_rating, c.to_par AS course_par
        FROM rounds r
        INNER JOIN courses c
          ON c.tees = r.tees
         AND c.course_name = r.course_name
         AND c.hole = r.hole
        INNER JOIN players p
          ON r.GHIN = p.GHIN
         AND r.handicap_index = p.handicap_index
         AND r.date = p.date;
        "
      )
      
      # construct round-level summary (scores_sum)
      scores_sum <- scores |>
        dplyr::mutate(
          date = lubridate::as_date(.data$date),
          hole = as.numeric(gsub("hole_", "", .data$hole)),
          date_course = paste0(.data$date, "\n", .data$course_name, "\n", .data$handicap_index),
          score_rel_par = .data$tot_gross - .data$course_par
        ) |>
        dplyr::rename(
          `Handicap Index` = .data$handicap_index,
          `Gross Score` = .data$tot_gross,
          course = .data$course_name
        ) |>
        dplyr::distinct(
          .data$date,
          .data$date_course,
          .data$`Handicap Index`,
          .data$course,
          .data$tees,
          .data$course_rating,
          .data$`Gross Score`,
          .data$score_rel_par,
          .data$course_par
        )
      
      {
        tmp_q <- try(DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM predictions_round;"), silent = TRUE)
        if (inherits(tmp_q, "try-error")) {
          prod_pred_before <<- NA_integer_
        } else {
          prod_pred_before <<- tmp_q$n
        }
        }
      
      # fit LMER
      gross_lmer <- tryCatch(
        {
          lme4::lmer(
            `Gross Score` ~ `Handicap Index` * course_rating + days + (1 | course),
            data = scores_sum |>
              dplyr::mutate(
                course_rating = .data$course_rating - mean(.data$course_rating),
                course = gsub(.data$date_course,
                              pattern = "[0-9]|\\-|\\\n|\\.",
                              replacement = ""),
                `Handicap Index` = -.data$`Handicap Index` - mean(-.data$`Handicap Index`),
                days = as.numeric(as.Date(.data$date) - min(as.Date(.data$date)) + 1,
                                  units = "days")
              )
          )
        },
        warning = function(w) {
          message("LMER WARNING (IGNORED): ", conditionMessage(w))
          invokeRestart("muffleWarning")
        },
        error = function(e) {
          stop(e)
        }
      )
      
      # add LMER predictions + residuals (expected_gross is relative-to-par here)
      scores_sum <- scores_sum |>
        dplyr::mutate(
          expected_gross = round(as.numeric(stats::predict(gross_lmer)), 2),
          .after = .data$`Gross Score`
        ) |>
        dplyr::mutate(
          expected_gross = .data$expected_gross - .data$course_par,
          residual = .data$score_rel_par - .data$expected_gross
        )
      
      # fit KF on residuals
      y <- scores_sum$residual
      init_logQ <- log(stats::var(y, na.rm = TRUE))
      init_logH <- log(stats::var(y, na.rm = TRUE))
      
      build_kf_model <- function(pars, model) {
        KFAS::SSModel(
          residual ~ SSMtrend(1, Q = list(exp(pars[1]))),
          H = exp(pars[2]),
          data = scores_sum
        )
      }
      
      # initialize the model
      mod0 <- build_kf_model(c(init_logQ, init_logH), NULL)
      
      # pass the data forward
      fit <- KFAS::fitSSM(
        model = mod0,
        inits = c(init_logQ, init_logH),
        updatefn = build_kf_model,
        method = "BFGS"
      )
      
      # smooth back over the data
      kfs <- KFAS::KFS(fit$model, smoothing = "state")
      trend_index <- ncol(kfs$alphahat)
      
      scores_sum <- scores_sum |>
        dplyr::mutate(
          skill_est = as.numeric(kfs$alphahat[, trend_index]),
          skill_var = as.numeric(kfs$V[trend_index, trend_index, ]),
          skill_ci_lower = .data$skill_est - 2 * sqrt(.data$skill_var),
          skill_ci_upper = .data$skill_est + 2 * sqrt(.data$skill_var)
        )
      
      # map latent skill to handicap scale
      scores_sum <- scores_sum |>
        dplyr::mutate(
          idx_analog = .data$expected_gross + .data$skill_est
        )
      
      # re-scale the true talent estimate to the USGA handicap index
      fit_hi <- stats::lm(`Handicap Index` ~ idx_analog, data = scores_sum)
      
      # timestamp-based version identifier
      model_version <- format(Sys.time(), "%Y%m%d_%H%M%S")
      generated_at  <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
      
      scores_sum <- scores_sum |>
        dplyr::mutate(
          KF_idx = stats::predict(
            fit_hi,
            newdata = scores_sum |>
              dplyr::mutate(idx_analog = .data$expected_gross + .data$skill_est)
          ) |>
            as.numeric(),
          KF_gross_hat = .data$KF_idx + .data$course_par,
          sg_self = .data$KF_gross_hat - .data$`Gross Score`,
          expected_rel_par = .data$expected_gross,
          skill_adj_gross = .data$expected_gross + .data$course_par + .data$skill_est,
          skill_adj_rel_par = .data$skill_adj_gross - .data$course_par,
          skill_rel_par = .data$skill_adj_rel_par,
          index_true = .data$KF_idx,
          index_posted = .data$`Handicap Index`,
          index_gap = .data$index_true - .data$index_posted,
          model_version = model_version,
          generated_at = generated_at,
          features_hash = NA_character_,
          notes = NA_character_
        ) |> 
        dplyr::rename(course_name = .data$course)
      
      # build predictions_round tibble
      predictions_round <- scores_sum |>
        dplyr::transmute(
          .data$date,
          .data$course_name,
          .data$tees,
          .data$course_rating,
          .data$course_par,
          gross_score = .data$`Gross Score`,
          expected_gross = .data$expected_gross + .data$course_par,
          .data$expected_rel_par,
          .data$residual,
          .data$skill_est,
          .data$skill_var,
          .data$skill_ci_lower,
          .data$skill_ci_upper,
          .data$skill_adj_gross,
          .data$skill_adj_rel_par,
          .data$skill_rel_par,
          .data$sg_self,
          .data$index_posted,
          .data$index_true,
          .data$index_gap,
          .data$model_version,
          .data$generated_at,
          .data$features_hash,
          .data$notes
        )
      
      lm_rows <<- nrow(predictions_round)
      kf_rows <<- nrow(predictions_round)
      
      # predictions production table ----
      
      if (DBI::dbIsValid(con)) {
        DBI::dbWriteTable(
          con,
          "predictions_round",
          predictions_round,
          append = TRUE
        )
      }
      
      {
        tmp_q2 <- try(DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM predictions_round;"), silent = TRUE)
        if (inherits(tmp_q2, "try-error")) {
          prod_pred_after <<- NA_integer_
        } else {
          prod_pred_after <<- tmp_q2$n
        }
      }
      
      # dev tables ----
      
      # scores sum ----
      if (DBI::dbIsValid(con)) {
        DBI::dbWriteTable(
          con,
          "dev_scores_sum",
          scores_sum,
          overwrite = TRUE
        )
      }
      
      # predictions ----
      if (DBI::dbIsValid(con)) {
        DBI::dbWriteTable(
          con,
          "dev_predictions_round",
          predictions_round,
          overwrite = TRUE
        )
      }
      
      # rounds ----
      dev_rounds <- NULL
      {
        tmp_r <- try(DBI::dbReadTable(con, "rounds"), silent = TRUE)
        if (!inherits(tmp_r, "try-error")) {
          dev_rounds <- tmp_r
        }
      }
      if (DBI::dbIsValid(con) && !is.null(dev_rounds)) {
        DBI::dbWriteTable(
          con,
          "dev_rounds",
          dev_rounds,
          overwrite = TRUE
        )
      }
      
      # players ----
      dev_players <- NULL
      {
        tmp_p <- try(DBI::dbReadTable(con, "players"), silent = TRUE)
        if (!inherits(tmp_p, "try-error")) {
          dev_players <- tmp_p
        }
      }
      if (DBI::dbIsValid(con) && !is.null(dev_players)) {
        DBI::dbWriteTable(
          con,
          "dev_players",
          dev_players,
          overwrite = TRUE
        )
      }
      
      # courses ----
      dev_courses <- NULL
      {
        tmp_c <- try(DBI::dbReadTable(con, "courses"), silent = TRUE)
        if (!inherits(tmp_c, "try-error")) {
          dev_courses <- tmp_c
        }
      }
      if (DBI::dbIsValid(con) && !is.null(dev_courses)) {
        DBI::dbWriteTable(
          con,
          "dev_courses",
          dev_courses,
          overwrite = TRUE
        )
      }
      
      # club_metrics ----
      dev_club_metrics <- NULL
      {
        tmp_m <- try(DBI::dbReadTable(con, "club_metrics"), silent = TRUE)
        if (!inherits(tmp_m, "try-error")) {
          dev_club_metrics <- tmp_m
        }
      }
      if (DBI::dbIsValid(con) && !is.null(dev_club_metrics)) {
        DBI::dbWriteTable(
          con,
          "dev_club_metrics",
          dev_club_metrics,
          overwrite = TRUE
        )
      }
      
      print("TABLES INSIDE TRY, AFTER DEV WRITES:")
      print(DBI::dbListTables(con))
      
      print("INSIDE TRY BLOCK - END")
      
      
    }, silent = TRUE)
    
    
    if (inherits(tmp, "try-error")) {
      pipeline_error <- TRUE
      pipeline_error_msg <- as.character(tmp)
    }
  }
  
  # pipeline log ----
  if (pipeline_error) {
    status <- "error"
    err_msg <- pipeline_error_msg
  }
  
  log_row <- tibble::tibble(
    run_timestamp = run_started,
    status = status,
    message = err_msg,
    lm_rows = lm_rows,
    kf_rows = kf_rows,
    prod_pred_before = prod_pred_before,
    prod_pred_after = prod_pred_after
  )
  
  
  if (DBI::dbIsValid(con)) {
    DBI::dbWriteTable(
      con,
      "pipeline_run_log",
      log_row,
      append = TRUE
    )
  }
  
  if (!is.null(predictions_round) && identical(status, "success")) {
    invisible(predictions_round)
  } else {
    invisible(NULL)
  }
}

