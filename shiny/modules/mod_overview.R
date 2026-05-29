mod_overview_ui <- function(id) {
  ns <- shiny::NS(id)
  
  bslib::layout_sidebar(
    
    sidebar = shiny::tagList(
      
      shiny::dateRangeInput(
        inputId = ns("date_range"),
        label = "Date Range:",
        start = NULL,
        end = NULL
      ),
      
      shiny::selectInput(
        ns("ts_choice"),
        "Time Series:",
        choices = c(
          "Gross Score",
          "Net Score",
          "Handicap Index",
          "'Skill' Curve"
        ),
        selected = "Gross Score"
      )
    ),
    
    main = shiny::tagList(
      
      bslib::layout_column_wrap(
        width = "200px",
        shiny::uiOutput(ns("kpi_score")),
        shiny::uiOutput(ns("kpi_net")),
        shiny::uiOutput(ns("kpi_index")),
        shiny::uiOutput(ns("kpi_fir")),
        shiny::uiOutput(ns("kpi_gir")),
        shiny::uiOutput(ns("kpi_putts"))
      ),
      
      shiny::hr(),
      
      apexcharter::apexchartOutput(ns("ts_plot"), height = "400px")
    )
  )
}


mod_overview_server <- function(id, data) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Filter data by date range
    filtered_data <- shiny::reactive({
      req(input$date_range)
      dplyr::filter(
        data(),
        date >= input$date_range[1],
        date <= input$date_range[2]
      )
    })
    
    # KPI values
    kpi_vals <- shiny::reactive({
      list(
        score     = compute_kpi_score(filtered_data()),
        net       = compute_kpi_net(filtered_data()),
        index     = compute_kpi_index(filtered_data()),
        fir       = compute_kpi_fir(filtered_data()),
        gir       = compute_kpi_gir(filtered_data()),
        putts     = compute_kpi_putts(filtered_data())
      )
    })
    
    # KPI outputs
    output$kpi_score <- shiny::renderUI({
      kpi_card("Avg Score", kpi_vals()$score)
    })
    
    output$kpi_net <- shiny::renderUI({
      kpi_card("Avg Net", kpi_vals()$net)
    })
    
    output$kpi_index <- shiny::renderUI({
      kpi_card("HI", round(kpi_vals()$index, 1))
    })
    
    output$kpi_fir <- shiny::renderUI({
      kpi_card("FIR %", paste0(round(kpi_vals()$fir * 100, 1), "%"))
    })
    
    output$kpi_gir <- shiny::renderUI({
      kpi_card("GIR %", paste0(round(kpi_vals()$gir * 100, 1), "%"))
    })
    
    output$kpi_putts <- shiny::renderUI({
      kpi_card("Putts", round(kpi_vals()$putts, 1))
    })
    
    # Time series: gross score
    gross_score_ts <- shiny::reactive({
      df <- compute_gross_score_ts(filtered_data())
      apexcharter::apex(
        data = df,
        mapping = apexcharter::aes(x = date, y = score)
      ) |>
        apexcharter::ax_chart(type = "line") |>
        apexcharter::ax_stroke(width = 3) |>
        apexcharter::ax_markers(size = 4) |>
        apexcharter::ax_tooltip(shared = TRUE)
    })
    
    # Time series: net score
    net_score_ts <- shiny::reactive({
      df <- compute_net_score_ts(filtered_data())
      apexcharter::apex(
        data = df,
        mapping = apexcharter::aes(x = date, y = net)
      ) |>
        apexcharter::ax_chart(type = "line") |>
        apexcharter::ax_stroke(width = 3) |>
        apexcharter::ax_markers(size = 4) |>
        apexcharter::ax_tooltip(shared = TRUE)
    })
    
    # Time series: handicap index
    index_ts <- shiny::reactive({
      df <- compute_index_ts(filtered_data())
      apexcharter::apex(
        data = df,
        mapping = apexcharter::aes(x = date, y = index)
      ) |>
        apexcharter::ax_chart(type = "line") |>
        apexcharter::ax_stroke(width = 3) |>
        apexcharter::ax_markers(size = 4) |>
        apexcharter::ax_tooltip(shared = TRUE)
    })
    
    # Time series: skill curve
    skill_curve_ts <- shiny::reactive({
      df <- compute_skill_curve_ts(filtered_data())
      apexcharter::apex(
        data = df,
        mapping = apexcharter::aes(x = date, y = skill)
      ) |>
        apexcharter::ax_chart(type = "line") |>
        apexcharter::ax_stroke(width = 3, curve = "smooth") |>
        apexcharter::ax_markers(size = 4) |>
        apexcharter::ax_tooltip(shared = TRUE)
    })
    
    # Render selected plot
    output$ts_plot <- apexcharter::renderApexchart({
      switch(
        input$ts_choice,
        "Gross Score"     = gross_score_ts(),
        "Net Score"       = net_score_ts(),
        "Handicap Index"  = index_ts(),
        "'Skill' Curve"     = skill_curve_ts()
      )
    })
  })
}


# KPI card helper (CSS-based)
kpi_card <- function(label, value) {
  shiny::div(
    class = "kpi-card",
    shiny::h4(label),
    shiny::h2(value)
  )
}
