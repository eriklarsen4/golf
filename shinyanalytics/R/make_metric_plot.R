# metric plots ----
# make_metric_plot <- function(df, metric) {
#   if (!metric %in% names(df)) {
#     stop("Column not found: ", metric)
#   }
#   
#   label_map <- c(
#     fir       = "FIR %",
#     gir       = "GIR %",
#     updown   = "Up & Down %",
#     total_putts     = "Putts per Round",
#     total_penalties = "Penalties per Round"
#   )
#   
#   ylab <- label_map[[metric]]
#   
#   df2 <- df |>
#     dplyr::arrange(date) |>
#     dplyr::select(date, value = dplyr::all_of(metric))
#   
#   ggplot2::ggplot(
#     df2,
#     ggplot2::aes(x = date, y = value)
#   ) +
#     ggplot2::geom_point(size = 3, alpha = 0.8) +
#     ggplot2::geom_smooth(method = "lm", linewidth = 1.2) +
#     ggplot2::labs(
#       title = metric,
#       x = "Date",
#       y = metric
#     )
# }

# make_metric_plot <- function(df,
#                              x,
#                              y,
#                              facet_var = NULL,
#                              smooth = FALSE,
#                              flip_axes = FALSE,
#                              xlab = NULL,
#                              ylab = NULL) {
#   
#   if (!x %in% names(df)) stop("Column not found: ", x)
#   if (!y %in% names(df)) stop("Column not found: ", y)
#   
#   if (is.null(facet_var)) {
#     df2 <- df |>
#       purrr::transmute(
#         xval = .data[[x]],
#         yval = .data[[y]]
#       )
#   } else {
#     df2 <- df |>
#       purrr::transmute(
#         xval = .data[[x]],
#         yval = .data[[y]],
#         facet = .data[[facet_var]]
#       )
#   }
#   
#   p <- ggplot2::ggplot(df2, ggplot2::aes(x = xval, y = yval)) +
#     ggplot2::geom_point(size = 3, alpha = 0.8)
#   
#   if (inherits(df2$xval, "Date")) {
#     p <- p + ggplot2::scale_x_date(
#       date_breaks = '2 weeks',
#       date_labels = '%b %d'
#     )
#   }
#   
#   if (smooth) {
#     p <- p + ggplot2::geom_smooth(method = "lm", linewidth = 1.2)
#   }
#   
#   if (!is.null(facet_var)) {
#     p <- ggplot2::ggplot(df2, ggplot2::aes(x = xval, y = yval, color = facet)) +
#       ggplot2::geom_point(size = 3, alpha = 0.8)
#   } else {
#     p <- ggplot2::ggplot(df2, ggplot2::aes(x = xval, y = yval)) +
#       ggplot2::geom_point(size = 3, alpha = 0.8)
#   }
#   
#   if (flip_axes) {
#     p <- p + ggplot2::coord_flip()
#   }
#   
#   p + ggplot2::labs(
#     x = xlab %||% x,
#     y = ylab %||% y
#   )
# }

make_metric_plot <- function(df_round, df_club, metric, facet, smooth, flip, x_choice, window_df = NULL) {
  
  req(metric)
  req(facet)
  req(x_choice)
  
  if (is.null(df_round)) {
    p <- ggplot2::ggplot() +
      ggplot2::annotate(
        geom  = "text",
        x     = 0.5,
        y     = 0.5,
        label = "No data available.",
        size  = 6
      ) +
      ggplot2::theme_bw()
    
    return(p)
  }
  
  if (!is.null(facet) &&
      facet == "Tee Club" &&
      !(metric %in% c("FIR % by Tee Club", "GIR % by Tee Club"))) {
    validate("Tee Club faceting is only available for tee‑club metrics.")
  }
  
  metric_map <- c(
    "FIR %"                   = "fir",
    "GIR %"                   = "gir",
    "Up & Down %"             = "updown",
    "Putts per Round"         = "tot_putts",
    "Chips per Round"         = "tot_chips",
    "Putts + Chips per Round" = "tot_putts_and_chips",
    "Penalties per Round"     = "tot_penalties",
    "Gross Score"             = "tot_gross",
    
    "FIR % by Tee Club"          = "tee_club_fir",
    "GIR % by Tee Club"          = "tee_club_gir"
  )
  
  facet_map <- c(
    "None"     = NA_character_,
    "Course"   = "course_name",
    "Tee Club" = "tee_club"
  )
  
  metric_col <- metric_map[[metric]]
  facet_var  <- facet_map[[facet]]
  
  # club metric detection aligned with UI
  is_club_metric <- metric_col %in% c("tee_club_fir", "tee_club_gir")
  
  if (x_choice == "Date") {
    x_var <- "date"
    x_lab <- "Date"
    y_var <- metric_col
  } else {
    x_var <- metric_col
    x_lab <- metric
    y_var <- "tot_gross"
  }
  
  # df selection aligned with UI semantics
  if (isTRUE(facet_var == "Tee Club")) {
    df <- df_club |> dplyr::filter(!is.na(tee_club))
  } else if (is_club_metric) {
    df <- df_club |> dplyr::filter(!is.na(tee_club))
  } else {
    df <- df_round
  }
  
  
  if (!is.null(window_df)) {
    
    window_start <- window_df$window_start
    window_end <- window_df$window_end
    
    max_date <- max(df$date, na.rm = T)
    
    df_window <- df |> 
      dplyr::filter(
        date >= max_date - window_end,
        date <= max_date - window_start
      )
    
    if (is_club_metric || (!is.null(facet) && facet == "Tee Club")) {
      df <- df_window |> dplyr::filter(!is.na(tee_club))
    } else {
      df <- df_window
    }
  }
  
  if ("tee_club" %in% names(df)) {
    df$tee_club <- factor(
      df$tee_club,
      levels = c("D", "3W", "4", "5", "6", "7", "8", "9", "PW", "GW", "SW", "LW")
    )
  }
  if ("par" %in% names(df)) {
    df$par <- factor(
      df$par,
      levels = c(3, 4, 5),
      labels = c("Par 3", "Par 4", "Par 5")
    )
  }
  
  if (is.null(df) || nrow(df) < 2) {
    p <- ggplot2::ggplot() +
      ggplot2::annotate(
        geom  = "text",
        x     = 0.5,
        y     = 0.5,
        label = "Not enough data in this window.",
        size  = 6
      ) +
      ggplot2::theme_bw()
    
    return(p)
  }
  
  # base plot
  
  if (isTRUE(is.na(facet_var))) {
    p <- ggplot2::ggplot(
      df,
      ggplot2::aes(
        x = .data[[x_var]],
        y = .data[[y_var]]
      )
    )
  } else {
    p <- ggplot2::ggplot(
      df,
      ggplot2::aes(
        x     = .data[[x_var]],
        y     = .data[[y_var]],
        color = .data[[facet_var]]
      )
    )
  }
  
  # geoms
  
  p <- p + ggplot2::geom_point(size = 2, alpha = 0.8)
  
  if (smooth) {
    p <- p + ggplot2::geom_smooth(method = "lm",
                                  linewidth = 0.6,
                                  inherit.aes = FALSE,
                                  aes(x = .data[[x_var]], y = .data[[y_var]]))
  }
  
  # faceting
  
  if (!is.na(facet_var) && facet_var == "tee_club") {
    if ( metric_col == "tee_club_gir" && "par" %in% names(df)) {
      p <- p + ggplot2::facet_grid(rows = vars(par), cols = vars(tee_club))
    } else {
      p <- p + ggplot2::facet_wrap(~tee_club)
    }
  }
  
  # flip?
  
  if (flip) {
    p <- p + ggplot2::coord_flip()
  }
  
  # labs
  
  if (x_choice == "Date") {
    final_x_lab <- "Date"
    final_y_lab <- metric
  } else {
    final_x_lab <- metric
    final_y_lab <- "Gross Score"
  }
  
  if (isTRUE(is.na(facet_var))) {
    p <- p + ggplot2::labs(
      x = final_x_lab,
      y = final_y_lab
    )
  } else if (x_choice == 'Date' && facet_var == 'tee_club') {
    p <- p + ggplot2::labs(
      x = final_x_lab,
      y = final_y_lab,
      color = facet,
    ) +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(angle = 270, hjust = 0.5, vjust = 0.5)
      ) + 
      ggplot2::coord_cartesian(ylim = range(df[[y_var]], na.rm = T))
  } else {
    p <- p + ggplot2::labs(
      x = final_x_lab,
      y = final_y_lab,
      color = facet
    ) +
      ggplot2::coord_cartesian(ylim = range(df[[y_var]], na.rm = T)) +
      ggplot2::theme(
        legend.position = 'bottom'
      )
  }
  
  p
}


