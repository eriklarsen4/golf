mod_lie_ui <- function(id) {
  ns <- shiny::NS(id)
  
  # shiny::tabPanel(
  #   title = "Lie Performance",
  shiny::sidebarLayout(
    shiny::sidebarPanel(
      width = 3
    ),
    
    shiny::mainPanel(
      width = 9,
      class = 'main-panel',
      
      shiny::tabsetPanel(
        
        shiny::tabPanel(
          title = "Distance Loss by Lie on Full Strokes",
          
          shiny::div(
            style = "width: 100%;",
            # style = "height: 40vh; min-height: 250px;",
            plotly::plotlyOutput(
              outputId = ns("distance_loss")#,
              # height = "auto"
            )
          )
        ),
        
        shiny::tabPanel(
          title = "Miss Direction Composition by Lie",
          
          shiny::selectInput(
            inputId = ns("miss_view"),
            label   = "View",
            choices = c("proportion", "count", "heatmap"),
            selected = "proportion",
            multiple = F
          ),
          
          shiny::div(
            style = "width: 100%;",
            # style = "height: 40vh; min-height: 250px;",
            plotly::plotlyOutput(
              outputId = ns("miss_by_lie")#,
              # height = "auto"
            )
          )
        ),
        
        shiny::tabPanel(
          title = "Miss Direction Composition by Lie & Club",
          
          shiny::selectInput(
            inputId = ns("miss_view_lie_club"),
            label   = "View",
            choices = c("proportion", "count", "heatmap"),
            selected = "proportion",
            multiple = F
          ),
          
          shiny::div(
            style = "width: 100%;",
            # style = "height: 42vh; min-height: 240px;",
            plotly::plotlyOutput(
              outputId = ns("miss_by_lie_club"),
              # height = "auto"
            )
          )
          
        )
      )
    )
  )
  # )
}


mod_lie_server <- function(id, stroke_level_df, stroke_quality) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    lie_levels = c("tee", "fairway", "rough")
    
    # Distance Loss by Lie
    output$distance_loss <- plotly::renderPlotly({
      req(stroke_quality())
      
      df <- stroke_quality() |>
        dplyr::ungroup() |>
        dplyr::filter(!is.na(lie),
                      lie %in% lie_levels) |>
        dplyr::filter(grepl(shot_type, pattern = 'full|tee')) |> 
        dplyr::mutate(
          lie = factor(lie, levels = c("tee", "fairway", "rough")),
          distance_loss = yd_diff
        ) |> droplevels()
      
      p <- ggplot2::ggplot(
        df,
        ggplot2::aes(
          x    = lie,
          y    = distance_loss,
          fill = lie
        )
      ) +
        ggplot2::geom_boxplot(
          alpha = 0.6,
          outlier.shape = NA, 
          width = 0.5) +
        ggplot2::stat_boxplot(
          geom = "errorbar", 
          width = 0) +
        # ggplot2::geom_boxplot(
        #   alpha         = 0.6,
        #   outlier.shape = NA,
        #   # outlier.alpha = 0.4,
        #   coef = 0,
        #   varwidth = FALSE,
        #   width         = 0.7
        # ) +
        ggplot2::geom_hline(
          yintercept = 0,
          linewidth  = 0.7,
          linetype   = "dashed",
          color      = "gray40"
        ) +
        ggplot2::labs(
          title    = "Distance Loss\non Full Swings",
          subtitle = "target − actual distance",
          x        = "Lie Type",
          y        = "Distance Loss (yd)"
        ) +
        ggplot2::theme(
          aspect.ratio = NULL,
          plot.margin = ggplot2::margin(5,5,5,5)#,
          # axis.text.x = ggplot2::element_text(angle = 270, hjust = 0.5, vjust = 0.5)
        )
        ggplot2::coord_flip()
      
      plotly::ggplotly(p)
    })
    
    
    # Miss Direction by Lie
    output$miss_by_lie <- plotly::renderPlotly({
      req(stroke_level_df())
      
      df <- stroke_level_df() |>
        dplyr::filter(
          shot_type %in% c("full", "tee"),
          !is.na(lie),
          !is.na(miss_direction)
        ) |> 
        dplyr::mutate(miss_direction = factor(miss_direction, levels = c("left", "right", "short", "long", "on_target")),
                      club = factor(club, levels = c("LW", "SW", "GW", "PW", "9", "8", "7", "6", "5", "4", "3W", "D")))
      
      view <- input$miss_view
      
      if (view == "proportion") {
        df2 <- df |>
          dplyr::count(lie, miss_direction) |>
          dplyr::group_by(lie) |>
          dplyr::mutate(pct = n / sum(n, na.rm = T)) |>
          dplyr::ungroup()
        
        return(
          ggplot2::ggplot(
            df2,
            ggplot2::aes(x = lie, y = pct, fill = miss_direction)
          ) +
            ggplot2::geom_col(alpha = 0.85) +
            ggplot2::labs(
              x = "Lie",
              y = "Proportion",
              fill = "Miss\nDirection",
              title = "Miss Direction Composition"
            ) +
            ggplot2::theme(legend.position = 'bottom',
                           aspect.ratio = NULL,
                           margins = ggplot2::margin(5,5,5,5),
                           title = ggplot2::element_text(size = 12),
                           axis.text = ggplot2::element_blank())
        )
      }
      
      if (view == "count") {
        df2 <- df |>
          dplyr::count(lie, miss_direction)
        
        return(
          ggplot2::ggplot(
            df2,
            ggplot2::aes(x = lie, y = n, fill = miss_direction)
          ) +
            ggplot2::geom_col(alpha = 0.85) +
            ggplot2::labs(
              x = "Lie",
              y = "Count",
              fill = "Miss\nDirection",
              title = "Miss Direction Counts"
            ) +
            ggplot2::theme(legend.position = 'bottom',
                           aspect.ratio = NULL,
                           margins = ggplot2::margin(5,5,5,5),
                           title = ggplot2::element_text(size = 12))
        )
      }
      
      if (view == "heatmap") {
        df2 <- df |>
          dplyr::count(lie, miss_direction)
        
        return(
          ggplot2::ggplot(
            df2,
            ggplot2::aes(
              x = miss_direction,
              y = lie,
              fill = n
            )
          ) +
            ggplot2::geom_tile(color = "white") +
            ggplot2::scale_fill_viridis_c() +
            ggplot2::labs(
              x = "Miss Direction",
              y = "Lie",
              fill = "Count",
              title = "Miss Direction Heatmap"
            ) +
            ggplot2::theme(legend.position = 'bottom',
                           aspect.ratio = NULL,
                           margins = ggplot2::margin(5,5,5,5),
                           axis.text.x = ggplot2::element_text(angle = 270, vjust = 0.5, hjust = 0.5))
        )
      }
    })
    
    # Miss Direction by Lie & Club
    output$miss_by_lie_club <- plotly::renderPlotly({
      req(stroke_level_df())
      
      df <- stroke_level_df() |>
        dplyr::filter(
          shot_type %in% c("full", "tee"),
          !is.na(lie),
          !is.na(club),
          !is.na(miss_direction)
        ) |> 
        dplyr::mutate(miss_direction = factor(miss_direction, levels = c("left", "right", "short", "long", "on_target")),
                      club = factor(club, levels = c("LW", "SW", "GW", "PW", "9", "8", "7", "6", "5", "4", "3W", "D")))
      
      view <- input$miss_view_lie_club
      
      if (view == "proportion") {
        df2 <- df |>
          dplyr::count(lie, club, miss_direction) |>
          dplyr::group_by(lie, club) |>
          dplyr::mutate(pct = n / sum(n, na.rm = T)) |>
          dplyr::ungroup()
        
        return(
          ggplot2::ggplot(
            df2,
            ggplot2::aes(x = club, y = pct, fill = miss_direction)
          ) +
            ggplot2::geom_col(alpha = 0.85) +
            ggplot2::facet_wrap(~ lie) +
            ggplot2::labs(
              x = "Club",
              y = "Proportion",
              fill = "Miss\nDirection",
              title = "Miss Direction Composition"
            ) +
            ggplot2::theme(legend.position = 'bottom',
                           aspect.ratio = NULL,
                           margins = ggplot2::margin(5,5,5,5),
                           axis.text.x = ggplot2::element_blank())
        )
      }
      
      if (view == "count") {
        df2 <- df |>
          dplyr::count(lie, club, miss_direction)
        
        return(
          ggplot2::ggplot(
            df2,
            ggplot2::aes(x = club, y = n, fill = miss_direction)
          ) +
            ggplot2::geom_col(alpha = 0.85) +
            ggplot2::facet_wrap(~ lie) +
            ggplot2::labs(
              x = "Club",
              y = "Count",
              fill = "Miss\nDirection",
              title = "Miss Direction Counts"
            ) +
            ggplot2::theme(legend.position = 'bottom',
                           aspect.ratio = NULL,
                           margins = ggplot2::margin(5,5,5,5),
                           axis.text.x = ggplot2::element_blank())
        )
      }
      
      if (view == "heatmap") {
        df2 <- df |>
          dplyr::count(lie, club, miss_direction)
        
        return(
          ggplot2::ggplot(
            df2,
            ggplot2::aes(
              x = miss_direction,
              y = club,
              fill = n
            )
          ) +
            ggplot2::geom_tile(color = "white") +
            ggplot2::scale_fill_viridis_c() +
            ggplot2::facet_wrap(~ lie) +
            ggplot2::labs(
              x = "Miss Direction",
              y = "Club",
              fill = "Count",
              title = "Miss Direction Heatmap"
            ) +
            ggplot2::theme(legend.position = 'bottom',
                           aspect.ratio = NULL,
                           margins = ggplot2::margin(5,5,5,5),
                           axis.text.x = ggplot2::element_text(angle = 270, vjust = 0.5, hjust = 0.5),
                           axis.title.x.bottom = ggplot2::element_text(vjust = 2))
        )
      }
    })
  })
}
