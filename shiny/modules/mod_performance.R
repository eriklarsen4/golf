mod_performance_ui <- function(id) {
  ns <- shiny::NS(id)
  
  bslib::layout_sidebar(
    
    sidebar = shiny::tagList(
      
      shiny::selectInput(
        ns("metric_choice"),
        "Metric:",
        choices = c(
          "FIR %",
          "GIR %",
          "Up & Down %",
          "Putts per Round",
          "Penalties per Round"
        ),
        selected = "FIR %"
      )
    ),
    
    main = shiny::tagList(
      
      bslib::layout_column_wrap(
        width = "200px",
        shiny::uiOutput(ns("kpi_fir")),
        shiny::uiOutput(ns("kpi_gir")),
        shiny::uiOutput(ns("kpi_updown")),
        shiny::uiOutput(ns("kpi_putts")),
        shiny::uiOutput(ns("kpi_penalties"))
      ),
      
      shiny::hr(),
      
      shiny::plotOutput(ns("metric_plot"), height = "450px")
    )
  )
}


mod_performance_server <- function(id, data) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # KPI values (all per round)
    kpi_vals <- shiny::reactive({
      list(
        fir       = compute_kpi_fir(data()),
        gir       = compute_kpi_gir(data()),
        updown    = compute_kpi_updown(data()),
        putts     = compute_kpi_putts(data()),
        penalties = compute_kpi_penalties(data())
      )
    })
    
    # KPI outputs
    output$kpi_fir <- shiny::renderUI({
      kpi_card("FIR %", paste0(round(kpi_vals()$fir * 100, 1), "%"))
    })
    
    output$kpi_gir <- shiny::renderUI({
      kpi_card("GIR %", paste0(round(kpi_vals()$gir * 100, 1), "%"))
    })
    
    output$kpi_updown <- shiny::renderUI({
      kpi_card("Up & Down %", paste0(round(kpi_vals()$updown * 100, 1), "%"))
    })
    
    output$kpi_putts <- shiny::renderUI({
      kpi_card("Putts/Round", round(kpi_vals()$putts, 1))
    })
    
    output$kpi_penalties <- shiny::renderUI({
      kpi_card("Penalties/Round", round(kpi_vals()$penalties, 1))
    })
    
    # Scatterplot builder
    make_scatter <- function(df, yvar, facet_var) {
      ggplot2::ggplot(
        df,
        ggplot2::aes(x = date, y = .data[[yvar]])
      ) +
        ggplot2::geom_point(size = 3, alpha = 0.8) +
        ggplot2::geom_smooth(method = "lm", se = FALSE, linewidth = 1.2) +
        ggplot2::facet_wrap(ggplot2::vars(.data[[facet_var]])) +
        ggplot2::theme_minimal(base_size = 14)
    }
    
    # Render selected metric
    output$metric_plot <- plotly::renderPlotly({
      
      df <- data()
      
      p <- base::switch(
        input$metric_choice,
        
        "FIR %" = make_scatter(
          df,
          yvar = "fir",
          facet_var = "tee_club"
        ),
        
        "GIR %" = make_scatter(
          df,
          yvar = "gir",
          facet_var = "course_name"
        ),
        
        "Up & Down %" = make_scatter(
          df,
          yvar = "updown",
          facet_var = "course_name"
        ),
        
        "Putts per Round" = make_scatter(
          df,
          yvar = "putts_round",
          facet_var = "course_name"
        ),
        
        "Penalties per Round" = make_scatter(
          df,
          yvar = "penalties_round",
          facet_var = "course_name"
        )
      )
      
      plotly::ggplotly(
        p,
        tooltip = c("date", "fir", "gir", "updown", "putts_round", "penalties_round")
      )
    })
    
  })
}
