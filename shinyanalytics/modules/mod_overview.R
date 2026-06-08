mod_overview_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tabPanel(
    title = "Overview",
    
    shiny::sidebarLayout(
      
      shiny::sidebarPanel(
        width = 3,
        
        shiny::sliderInput(
          inputId = ns("date_range"),
          label   = "Date Range:",
          min     = as.Date("2000-01-01"),   # placeholder, overwritten in server
          max     = as.Date("2000-01-02"),   # placeholder, overwritten in server
          value   = c(as.Date("2000-01-01"), as.Date("2000-01-02")),
          timeFormat = "%Y-%m-%d"
        ),
        
        
        shiny::selectInput(
          inputId = ns("ts_choice"),
          label   = "Metric (Choose One):",
          choices = c(
            "Gross Score",
            "Net Score",
            "Handicap Index",
            "Skill Estimate"
          ),
          selected = "Gross Score",
          multiple = F
        )
      ),
      
      shiny::mainPanel(
        width = 9,
        
        class = 'main-panel',
        
        # KPI row (explicit, no bslib layout)
        shiny::div(
          style = "display: flex; flex-wrap: wrap; gap: 20px;",
          
          shiny::div(style = "width: 200px;", shiny::uiOutput(ns("kpi_score"))),
          shiny::div(style = "width: 200px;", shiny::uiOutput(ns("kpi_net"))),
          shiny::div(style = "width: 200px;", shiny::uiOutput(ns("kpi_index"))),
          shiny::div(style = "width: 200px;", shiny::uiOutput(ns("kpi_fir"))),
          shiny::div(style = "width: 200px;", shiny::uiOutput(ns("kpi_gir"))),
          shiny::div(style = "width: 200px;", shiny::uiOutput(ns("kpi_putts")))
        ),
        
        shiny::hr(),
        
        shiny::uiOutput(ns("ts_plot_container"), 
                        height = "auto",
                        style = "height: 40vh; min-height: 250px;"
                        )
      )
    )
  )
}

mod_overview_server <- function(id, data_r, data_skill) {
  shiny::moduleServer(id, function(input, output, session) {
    
    shiny::observe({
      df <- data_r()
        
      shiny::req(nrow(df) > 0)
      
      rng <- range(df$date, na.rm = T)
      
      shiny::updateSliderInput(
        session,
        "date_range",
        min   = rng[1],
        max   = rng[2],
        value = rng
      )
    })
    
    
    filtered <- shiny::reactive({
      shiny::req(input$date_range)
      
      base <- data_r() |>
        dplyr::mutate(date_js = as.numeric(as.Date(date)) * 86400000) |> 
        dplyr::filter(
          date >= input$date_range[1],
          date <= input$date_range[2]
        ) |> 
        dplyr::ungroup() |> 
        dplyr::mutate(date = as.Date(date)) |> 
        dplyr::select(date, date_js, course_name, tees, tot_gross, tot_net, handicap_index, fir, gir, tot_putts, tot_penalties) |> 
        dplyr::distinct()
      
      skill <- data_skill() |> 
        dplyr::ungroup() |> 
        dplyr::rename(tot_gross = gross_score,
                      handicap_index = index_posted) |> 
        dplyr::mutate(date = as.Date(date)) |> 
        dplyr::group_by(date, course_name, tees) |> 
        dplyr::slice_max(order_by = generated_at, n = 1) |> 
        dplyr::ungroup()
      
      dplyr::left_join(base,
                       skill, 
                       by = c('date', 'course_name', 'tees', 'handicap_index', 'tot_gross')) 
    })
    
    # KPIs
    output$kpi_score <- shiny::renderUI({
      df <- filtered()
      shiny::req(nrow(df) > 0)
      value <- round(mean(df$tot_gross, na.rm = T), 1)
      kpi_card("Avg. Gross:", value)
    })
    
    output$kpi_net <- shiny::renderUI({
      df <- filtered()
      shiny::req(nrow(df) > 0)
      value <- round(mean(df$tot_net, na.rm = T), 1)
      kpi_card("Avg. Net:", value)
    })
    
    output$kpi_index <- shiny::renderUI({
      df <- filtered()
      value <- round(mean(df$handicap_index, na.rm = T), 1)
      kpi_card("Avg. H.I.:", value)
    })
    
    output$kpi_fir <- shiny::renderUI({
      df <- filtered()
      value <- round(mean(df$fir, na.rm = T), 1)
      kpi_card("Avg. FIR %:", value)
    })
    
    output$kpi_gir <- shiny::renderUI({
      df <- filtered()
      value <- round(mean(df$gir, na.rm = T), 1)
      kpi_card("Avg. GIR %:", value)
    })
    
    output$kpi_putts <- shiny::renderUI({
      df <- filtered()
      value <- round(mean(df$tot_putts, na.rm = T), 1)
      kpi_card("Avg. Tot. Putts:", value)
    })
    
    ts_map <- list(
      "Gross Score"     = list(col = "tot_gross",       label = "Gross Score"),
      "Net Score"       = list(col = "tot_net",         label = "Net Score"),
      "Handicap Index"  = list(col = "handicap_index",  label = "Handicap Index"),
      "Skill Estimate"   = list(col = "skill_est",     label = "Skill Estimate")
    )
    
    # force ts rebuild
    output$ts_plot_container <- shiny::renderUI({
      input$ts_choice
      apexcharter::apexchartOutput(session$ns("ts_plot"), height = "auto",
                                   style  = "height: 40vh; min-height: 250px;")
    })
    
    # Time series plot
    output$ts_plot <- apexcharter::renderApexchart(height = function() 300, {
      shiny::req(input$ts_choice)
      spec <- ts_map[[input$ts_choice]]
      # message("metric = ", spec$col, " | label = ", spec$label)
      make_ts_plot(filtered(), metric = spec$col, label = spec$label)
    })
  })
}


