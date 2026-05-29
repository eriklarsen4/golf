mod_approach_ui <- function(id) {
  ns <- shiny::NS(id)
  
  bslib::layout_sidebar(
    
    sidebar = shiny::tagList(
      
      shiny::selectInput(
        ns("approach_view"),
        "View:",
        choices = c(
          "GIR Probability Curves",
          "Par-3 GIR % Over Time"
        ),
        selected = "GIR Probability Curves"
      ),
      
      shiny::selectInput(
        ns("lie_filter"),
        "Lie:",
        choices = c("All", "Fairway", "Rough", "Sand", "Tee"),
        selected = "All"
      )
    ),
    
    main = shiny::tagList(
      plotly::plotlyOutput(ns("approach_plot"), height = "450px")
    )
  )
}


mod_approach_server <- function(id, data) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Filter by lie if needed
    filtered_data <- shiny::reactive({
      if (input$lie_filter == "All") {
        data()
      } else {
        dplyr::filter(data(), lie == input$lie_filter)
      }
    })
    
    # GIR probability curves (LOESS)
    gir_curve_plot <- shiny::reactive({
      df <- filtered_data()
      
      p <- ggplot2::ggplot(
        df,
        ggplot2::aes(x = approach_distance, y = gir)
      ) +
        ggplot2::geom_point(alpha = 0.4, size = 2) +
        ggplot2::geom_smooth(
          method = "loess",
          linewidth = 1.2,
          color = "#003f87"
        ) +
        ggplot2::facet_wrap(ggplot2::vars(lie)) +
        ggplot2::labs(
          x = "Approach Distance (yards)",
          y = "GIR Probability"
        )
      
      plotly::ggplotly(
        p,
        tooltip = c("approach_distance", "gir", "lie")
      )
    })
    
    # Par-3 GIR% over time (LOESS)
    par3_plot <- shiny::reactive({
      df <- dplyr::filter(data(), par == 3)
      
      p <- ggplot2::ggplot(
        df,
        ggplot2::aes(x = date, y = gir)
      ) +
        ggplot2::geom_point(size = 3, alpha = 0.8) +
        ggplot2::geom_smooth(
          method = "loess",
          linewidth = 1.2
        ) +
        ggplot2::facet_wrap(ggplot2::vars(course_name)) +
        ggplot2::labs(
          x = "Date",
          y = "GIR % (Par 3)"
        )
      
      plotly::ggplotly(
        p,
        tooltip = c("date", "gir", "course_name")
      )
    })
    
    # Render selected view
    output$approach_plot <- plotly::renderPlotly({
      switch(
        input$approach_view,
        "GIR Probability Curves" = gir_curve_plot(),
        "Par-3 GIR % Over Time"  = par3_plot()
      )
    })
  })
}
