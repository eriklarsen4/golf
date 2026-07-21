mod_club_ui <- function(id) {
  ns <- shiny::NS(id)
  
  # shiny::tabPanel(
  #   title = "Club Diagnostics",
    
    shiny::sidebarLayout(
      
      shiny::sidebarPanel(
        width = 3,
        
        shiny::selectInput(
          inputId = ns("club_view"),
          label   = "View:",
          choices = c(
            "Shot Distance Window",
            "Full-Stroke Accuracy",
            "Miss Direction Composition",
            "Distance Control (Actual - Calibrated)",
            "Usage Distance Distribution"
          ),
          selected = "Shot Distance Window",
          multiple = FALSE
        )
      ),
      
      shiny::mainPanel(
        width = 9,
        class = 'main-panel',
        
        shiny::div(
          style = "width: 100%;",
          plotly::plotlyOutput(
            outputId = ns("club_plot")#,
            # height = "auto"
          )
        )
      )
    )
  # )
}

mod_club_server <- function(id, stroke_quality, full_stroke_quality_avg) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Calibration window tibble
    cal_windows <- tibble::tribble(
      ~club, ~ymin, ~ymax,
      "D", 260, 280,
      "3W", 235, 255,
      "4", 200, 220,
      "5", 190, 200,
      "6", 180, 190,
      "7", 170, 180,
      "8", 160, 170,
      "9", 150, 160,
      "PW", 135, 150,
      "GW", 110, 125,
      "SW", 50, 100
    )
    
    club_levels <- c("LW","SW","GW","PW","9","8","7","6","5","4","3W","D")
    
    # ---- 1. Shot Distance Window ----
    plot_calibration <- shiny::reactive({
      df <- full_stroke_quality_avg() |>
        dplyr::ungroup() |>
        dplyr::mutate(
          club = factor(club, levels = club_levels),
          x = as.numeric(club)
        ) |>
        dplyr::left_join(cal_windows, by = "club") |> 
        dplyr::group_by() |> 
        dplyr::mutate(
          target_center = (ymin + ymax) /2,
          lower_error = avg_yds_traveled - sd_yds_traveled,
          upper_error = avg_yds_traveled + sd_yds_traveled,
          min_shot_distance = min_yds_traveled,
          max_shot_distance = max_yds_traveled
        ) |> 
        dplyr::ungroup()
      
      p <- ggplot2::ggplot(
        df,
        ggplot2::aes(x = x, y = avg_yds_traveled, color = club, fill = club,
                     text = paste0(
                       "Club: ", club, "<br>",
                       "Avg: ", round(avg_yds_traveled, 1), " yd<br>",
                       "SD: ", round(sd_yds_traveled, 1), " yd<br>",
                       "Min Shot Dist.: ", min_shot_distance, " yd<br>",
                       "Max Shot Dist.: ", max_shot_distance, " yd<br>",
                       "Cal Min: ", round(ymin, 1), " yd<br>",
                       "Cal Max: ", round(ymax, 1), " yd<br>"
                     ))
      ) +
        ggplot2::geom_rect(
          ggplot2::aes(
            xmin = x - 0.45,
            xmax = x + 0.45,
            ymin = ymin,
            ymax = ymax
          ),
          fill = "black",
          alpha = 0.30,
          inherit.aes = F
        ) +
        ggplot2::geom_point(size = 3) +
        ggplot2::geom_errorbar(
          ggplot2::aes(
            ymin = avg_yds_traveled - sd_yds_traveled,
            ymax = avg_yds_traveled + sd_yds_traveled
          ),
          width = 0.15,
          linewidth = 0.7
        ) +
        ggplot2::scale_x_continuous(
          breaks = seq_along(club_levels),
          labels = club_levels,
          minor_breaks = NULL
        ) +
        ggplot2::labs(
          title = "Actual Shot Distance +\nCalibration",
          # subtitle = "Shaded bands show calibrated full-swing distance windows",
          x = "Club",
          y = "Mean Distance (yd)"
        ) +
        ggplot2::theme(
          axis.text.x = ggplot2::element_text(
            angle = 270,
            hjust = 0.5,
            vjust = 0.5,
          ),
          aspect.ratio = NULL,
          plot.margin = ggplot2::margin(5,5,5,5),
          
        )
      
      
      plotly::ggplotly(p, tooltip = 'text')
    })
    
    # ---- 2. Full-Stroke Accuracy ----
    plot_accuracy <- shiny::reactive({
      df <- stroke_quality() |>
        dplyr::filter(shot_type %in% c("full","tee")) |>
        dplyr::group_by(club) |>
        dplyr::summarize(
          accuracy = round((sum(on_target, na.rm = T) / dplyr::n()) * 100, 2),
          avg_yds_traveled = round(mean(yds_traveled, na.rm = T), 1),
          n = dplyr::n()
        ) |>
        dplyr::mutate(club = factor(club, levels = club_levels))
      
      p <- ggplot2::ggplot(
        df,
        ggplot2::aes(x = forcats::fct_reorder(club, avg_yds_traveled), y = accuracy, fill = club)
      ) +
        ggplot2::geom_col(alpha = 0.8) +
        ggplot2::geom_text(
          ggplot2::aes(label = paste0(accuracy, "%")),
          color = "black",
          vjust = -0.5,
          size = 4
        ) +
        ggplot2::geom_text(
          ggplot2::aes(label = paste0("\nn = ", n)),
          vjust = 1.5,
          size = 3,
          color = "black"
        ) +
        ggplot2::labs(
          title = "Accuracy on Full Swings",
          x = "Club",
          y = "Accuracy %"
        ) +
        ggplot2::theme(
          axis.text.x = ggplot2::element_text(
            angle = 270,
            hjust = 0.5,
            vjust = 0.5
          ),
          aspect.ratio = NULL,
          plot.margin = ggplot2::margin(5,5,5,5)
        )
      
      plotly::ggplotly(p)
    })
    
    # ---- 3. Miss Direction Composition ----
    plot_miss_direction <- shiny::reactive({
      df <- stroke_quality() |>
        dplyr::filter(shot_type %in% c("full","tee")) |>
        dplyr::count(club, miss_direction) |>
        dplyr::group_by(club) |>
        dplyr::mutate(
          club = factor(club, levels = club_levels),
          pct = round((n / sum(n, na.rm = T)) * 100, 1)
        ) |>
        dplyr::ungroup()
      
      p <- ggplot2::ggplot(
        df,
        ggplot2::aes(x = club, y = pct, fill = miss_direction)
      ) +
        ggplot2::geom_col(alpha = 0.85) +
        ggplot2::labs(
          title = "Miss Direction Composition",
          x = "Club",
          y = "Proportion of Miss Types (%)",
          fill = "Miss\nDirection"
        ) +
        ggplot2::theme(
          axis.text.x = ggplot2::element_text(
            angle = 270,
            hjust = 0.5,
            vjust = 0.5
          ),
          legend.position = 'bottom',
          aspect.ratio = NULL,
          plot.margin = ggplot2::margin(5,5,5,5)
        )
      
      plotly::ggplotly(p)
    })
    
    # ---- 4. Distance Control (Actual - Calibrated) ----
    plot_distance_control <- shiny::reactive({
      df <- full_stroke_quality_avg() |>
        dplyr::ungroup() |>
        dplyr::mutate(
          club = factor(club, levels = club_levels),
          x = as.numeric(club)
        ) |>
        dplyr::left_join(cal_windows, by = "club") |>
        dplyr::mutate(
          target_center = (ymin + ymax) / 2,
          distance_error = avg_yds_traveled - target_center
        )
      
      p <- ggplot2::ggplot(
        df,
        ggplot2::aes(x = x, y = distance_error, color = club)
      ) +
        ggplot2::geom_point(size = 3) +
        ggplot2::geom_errorbar(
          ggplot2::aes(
            ymin = (avg_yds_traveled - sd_yds_traveled) - target_center,
            ymax = (avg_yds_traveled + sd_yds_traveled) - target_center
          ),
          width = 0.15,
          linewidth = 0.7
        ) +
        ggplot2::geom_hline(
          yintercept = 0,
          linewidth = 0.7,
          linetype = "dashed",
          color = "gray40"
        ) +
        ggplot2::scale_x_continuous(
          breaks = seq_along(club_levels),
          labels = club_levels,
          minor_breaks = NULL
        ) +
        ggplot2::labs(
          title = "Distance Control (actual - stock)", 
          subtitle = "Difference between actual shot distance and stock distance",
          x = "Club",
          y = "Distance Error (yd)"
        ) +
        ggplot2::theme(
          axis.text.x = ggplot2::element_text(
            angle = 270,
            hjust = 0.5,
            vjust = 0.5
          ),
          aspect.ratio = NULL,
          plot.margin = ggplot2::margin(5,5,5,5),
          title = ggplot2::element_text(size = 12)
        )
      
      plotly::ggplotly(p)
    })
    
    # ---- 5. Ridgeline Usage Distance Distribution ----
    plot_ridgeline <- shiny::reactive({
      df <- stroke_quality() |>
        dplyr::filter(
          !is.na(club),
          club != '',
          club %in% club_levels) |> 
        dplyr::mutate(
          club = factor(club, levels = club_levels)
        )
      
      p <- ggplot2::ggplot(
        df,
        ggplot2::aes(
          x = yds_to_target,
          y = club,
          fill = club
        )
      ) +
        ggridges::geom_density_ridges(
          alpha = 0.6,
          scale = 1.2,
          color = "black",
          size = 0.3
        ) +
        ggplot2::labs(
          title = "Usage Distance Distribution",
          subtitle = "Shows how often each club is used from different distances",
          x = "Target Distance (yds)",
          y = "Club"
        ) +
        ggplot2::theme(
          aspect.ratio = NULL,
          plot.margin = ggplot2::margin(5,5,5,5),
          axis.text.x = ggplot2::element_text(angle = 270, hjust = 0.5, vjust = 0.5)
        )
      
      plotly::ggplotly(p)
    })
    
    # ---- Switch ----
    output$club_plot <- plotly::renderPlotly({
      req(input$club_view)
      req(stroke_quality())
      req(full_stroke_quality_avg())
      switch(
        input$club_view,
        "Shot Distance Window" = plot_calibration(),
        "Full-Stroke Accuracy" = plot_accuracy(),
        "Miss Direction Composition" = plot_miss_direction(),
        "Distance Control (Actual - Calibrated)" = plot_distance_control(),
        "Usage Distance Distribution" = plot_ridgeline()
      )
    })
  })
}
