lie_ui <- function(id) {
  ns <- shiny::NS(id)
  
  bslib::layout_sidebar(
    sidebar = NULL,
    
    bslib::card(
      shiny::tabsetPanel(
        
        shiny::tabPanel(
          "Distance Loss by Lie",
          shiny::plotOutput(ns("distance_loss"))
        ),
        
        shiny::tabPanel(
          "Miss Direction by Lie",
          shiny::selectInput(
            ns("miss_view"),
            "View",
            choices = c("proportion", "count", "heatmap"),
            selected = "proportion"
          ),
          shiny::plotOutput(ns("miss_by_lie"))
        ),
        
        shiny::tabPanel(
          "Miss Direction by Lie × Club",
          shiny::selectInput(
            ns("miss_view_lie_club"),
            "View",
            choices = c("proportion", "count", "heatmap"),
            selected = "proportion"
          ),
          shiny::plotOutput(ns("miss_by_lie_club"))
        )
      )
    )
  )
}


lie_server <- function(id, stroke_quality) {
  shiny::moduleServer(id, function(input, output, session) {
    
    # distance loss summary
    distance_loss_df <- shiny::reactive({
      stroke_quality |>
        dplyr::filter(grepl(shot_type, pattern = "full|tee")) |>
        dplyr::group_by(club, lie) |>
        dplyr::summarize(
          avg_yds_to_target = round(mean(yds_to_target), 1),
          avg_yds_traveled  = round(mean(yds_traveled), 1),
          .groups = "drop"
        ) |>
        dplyr::mutate(
          lie = factor(lie, levels = c("tee", "fairway", "rough", "fwbunker")),
          distance_loss = avg_yds_to_target - avg_yds_traveled
        )
    })
    
    output$distance_loss <- shiny::renderPlot({
      df <- distance_loss_df()
      
      ggplot2::ggplot(df, ggplot2::aes(x = lie, y = distance_loss, fill = lie)) +
        ggplot2::geom_boxplot(alpha = 0.6, outlier.alpha = 0.4, width = 0.7) +
        ggplot2::geom_hline(
          yintercept = 0,
          linewidth = 0.7,
          linetype = "dashed",
          color = "gray40"
        ) +
        ggplot2::labs(
          title = "Distance Loss by Lie Type",
          x = "Lie Type",
          y = "Distance Loss (yd)"
        ) +
        ggplot2::theme_bw(base_size = 14) +
        ggplot2::theme(
          axis.text = ggplot2::element_text(face = "bold", size = 10),
          axis.title = ggplot2::element_text(face = "bold", size = 11),
          panel.grid.major.x = ggplot2::element_blank(),
          panel.grid.minor.x = ggplot2::element_blank()
        )
    })
    
    # miss direction summary
    miss_by_lie_df <- shiny::reactive({
      stroke_quality |>
        dplyr::filter(grepl(shot_type, pattern = "full|tee")) |>
        dplyr::group_by(club, lie, miss_direction) |>
        dplyr::summarize(n = dplyr::n(), .groups = "drop") |>
        dplyr::filter(lie %in% c("tee", "fairway", "rough", "fwbunker")) |>
        dplyr::mutate(
          lie = factor(lie, levels = c("tee", "fairway", "rough", "fwbunker")),
          miss_direction = factor(
            miss_direction,
            levels = c("left", "right", "short", "long", "on_target")
          )
        )
    })
    
    output$miss_by_lie <- shiny::renderPlot({
      df <- miss_by_lie_df()
      view <- input$miss_view
      
      if (view == "heatmap") {
        df2 <- df |>
          dplyr::group_by(lie, miss_direction) |>
          dplyr::summarize(n = sum(n), .groups = "drop") |>
          dplyr::group_by(lie) |>
          dplyr::mutate(prop = n / sum(n)) |>
          dplyr::ungroup()
        
        ggplot2::ggplot(df2, ggplot2::aes(x = miss_direction, y = lie, fill = prop)) +
          ggplot2::geom_tile(color = "white") +
          ggplot2::scale_fill_gradient(low = "white", high = "steelblue") +
          ggplot2::labs(
            title = "Miss Direction Heatmap by Lie Type",
            x = "Miss Direction",
            y = "Lie Type",
            fill = "Proportion"
          ) +
          ggplot2::theme_bw(base_size = 14) +
          ggplot2::theme(
            axis.text = ggplot2::element_text(face = "bold", size = 10),
            axis.title = ggplot2::element_text(face = "bold", size = 11)
          )
        
      } else if (view == "count") {
        ggplot2::ggplot(df, ggplot2::aes(x = lie, fill = miss_direction)) +
          ggplot2::geom_bar(position = "stack", alpha = 0.85) +
          ggplot2::labs(
            title = "Miss Direction Counts by Lie Type",
            x = "Lie Type",
            y = "Count",
            fill = "Miss Direction"
          ) +
          ggplot2::theme_bw(base_size = 14) +
          ggplot2::theme(
            axis.text = ggplot2::element_text(face = "bold", size = 10),
            axis.title = ggplot2::element_text(face = "bold", size = 11)
          )
        
      } else {
        ggplot2::ggplot(df, ggplot2::aes(x = lie, fill = miss_direction)) +
          ggplot2::geom_bar(position = "fill", alpha = 0.85) +
          ggplot2::scale_y_continuous(
            labels = scales::percent_format(accuracy = 1),
            minor_breaks = NULL
          ) +
          ggplot2::labs(
            title = "Miss Direction Proportion by Lie Type",
            x = "Lie Type",
            y = "Proportion",
            fill = "Miss Direction"
          ) +
          ggplot2::theme_bw(base_size = 14) +
          ggplot2::theme(
            axis.text = ggplot2::element_text(face = "bold", size = 10),
            axis.title = ggplot2::element_text(face = "bold", size = 11)
          )
      }
    })
    
    # miss direction by lie × club
    output$miss_by_lie_club <- shiny::renderPlot({
      df <- miss_by_lie_df() |>
        dplyr::mutate(
          club = factor(
            club,
            levels = c("LW","SW","GW","PW","9","8","7","6","5","4","3W","D")
          )
        )
      
      view <- input$miss_view_lie_club
      
      if (view == "heatmap") {
        df2 <- df |>
          dplyr::group_by(lie, club, miss_direction) |>
          dplyr::summarize(n = sum(n), .groups = "drop") |>
          dplyr::group_by(lie, club) |>
          dplyr::mutate(prop = n / sum(n)) |>
          dplyr::ungroup()
        
        ggplot2::ggplot(df2, ggplot2::aes(x = miss_direction, y = club, fill = prop)) +
          ggplot2::geom_tile(color = "white") +
          ggplot2::scale_fill_gradient(low = "white", high = "steelblue") +
          ggplot2::facet_wrap(~ lie, nrow = 2) +
          ggplot2::labs(
            title = "Miss Direction Heatmap by Club × Lie Type",
            x = "Miss Direction",
            y = "Club",
            fill = "Proportion"
          ) +
          ggplot2::theme_bw(base_size = 14) +
          ggplot2::theme(
            axis.text = ggplot2::element_text(face = "bold", size = 10),
            axis.title = ggplot2::element_text(face = "bold", size = 11),
            strip.background = ggplot2::element_rect(fill = "white", color = "black")
          )
        
      } else if (view == "count") {
        ggplot2::ggplot(df, ggplot2::aes(x = club, fill = miss_direction)) +
          ggplot2::geom_bar(position = "stack", alpha = 0.85) +
          ggplot2::facet_wrap(~ lie, nrow = 2) +
          ggplot2::labs(
            title = "Miss Direction Counts by Club × Lie Type",
            x = "Club",
            y = "Count",
            fill = "Miss Direction"
          ) +
          ggplot2::theme_bw(base_size = 14) +
          ggplot2::theme(
            axis.text = ggplot2::element_text(face = "bold", size = 10),
            axis.title = ggplot2::element_text(face = "bold", size = 11),
            strip.background = ggplot2::element_rect(fill = "white", color = "black")
          )
        
      } else {
        ggplot2::ggplot(df, ggplot2::aes(x = club, fill = miss_direction)) +
          ggplot2::geom_bar(position = "fill", alpha = 0.85) +
          ggplot2::scale_y_continuous(
            labels = scales::percent_format(accuracy = 1),
            minor_breaks = NULL
          ) +
          ggplot2::facet_wrap(~ lie, nrow = 2) +
          ggplot2::labs(
            title = "Miss Direction Proportion by Club × Lie Type",
            x = "Club",
            y = "Proportion",
            fill = "Miss Direction"
          ) +
          ggplot2::theme_bw(base_size = 14) +
          ggplot2::theme(
            axis.text = ggplot2::element_text(face = "bold", size = 10),
            axis.title = ggplot2::element_text(face = "bold", size = 11),
            strip.background = ggplot2::element_rect(fill = "white", color = "black")
          )
      }
    })
  })
}
