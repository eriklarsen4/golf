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
          
          shiny::div(shiny::uiOutput(ns("mean_score"))),
          shiny::div(shiny::uiOutput(ns("best_score"))),
          
          shiny::div(shiny::uiOutput(ns("mean_net"))),
          shiny::div(shiny::uiOutput(ns("best_net"))),
          
          shiny::div(shiny::uiOutput(ns("mean_index"))),
          shiny::div(shiny::uiOutput(ns("best_index"))),
          
          shiny::div(shiny::uiOutput(ns("mean_fir"))),
          # shiny::div(shiny::uiOutput(ns("best_fir"))),
          
          shiny::div(shiny::uiOutput(ns("mean_gir"))),
          # shiny::div(shiny::uiOutput(ns("best_gir"))),
          
          shiny::div(shiny::uiOutput(ns("mean_putts")))#,
          # shiny::div(shiny::uiOutput(ns("best_putts")))
        ),
        
        shiny::hr(),
        
        shiny::div(
          #style = "height: 40vh; min-height: 250px;",
          shiny::uiOutput(ns("ts_plot_container")#, 
                          # height = "auto"
                          )
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
        dplyr::arrange(desc(date), desc(generated_at)) |> 
        dplyr::ungroup() |> 
        dplyr::group_by(date) |> 
        dplyr::slice_tail(n = 1) |> 
        dplyr::ungroup() |> 
        dplyr::arrange(desc(date), desc(generated_at)) |> 
        dplyr::distinct() |> 
        dplyr::rename(tot_gross = gross_score,
                      handicap_index = index_posted) |> 
        dplyr::mutate(date = as.Date(date)) # |> 
        # dplyr::group_by(date, course_name, tees) |> 
        # dplyr::slice_max(order_by = generated_at, n = 1) |> 
        # dplyr::ungroup()
      
      dplyr::left_join(base,
                       skill, 
                       by = c('date', 'course_name', 'tees', 'handicap_index', 'tot_gross'))
    })
    
    # KPIs
    output$mean_score <- shiny::renderUI({
      df <- filtered()
      shiny::req(nrow(df) > 0)
      value <- round(mean(df$tot_gross, na.rm = T), 1)
      kpi_card("Avg. Gross:", value)
    })
    
    output$mean_net <- shiny::renderUI({
      df <- filtered()
      shiny::req(nrow(df) > 0)
      value <- round(mean(df$tot_net, na.rm = T), 1)
      kpi_card("Avg. Net:", value)
    })
    
    output$mean_index <- shiny::renderUI({
      df <- filtered()
      value <- round(mean(df$handicap_index, na.rm = T), 1)
      kpi_card("Avg. H.I.:", value)
    })
    
    output$mean_fir <- shiny::renderUI({
      df <- filtered()
      value <- round(mean(df$fir, na.rm = T), 1)
      kpi_card("Avg. FIR %:", value)
    })
    
    output$mean_gir <- shiny::renderUI({
      df <- filtered()
      value <- round(mean(df$gir, na.rm = T), 1)
      kpi_card("Avg. GIR %:", value)
    })
    
    output$mean_putts <- shiny::renderUI({
      df <- filtered()
      value <- round(mean(df$tot_putts, na.rm = T), 1)
      kpi_card("Avg. Tot. Putts:", value)
    })
    
    
    output$best_score <- shiny::renderUI({
      df <- filtered()
      shiny::req(nrow(df) > 0)
      value <- round(min(df$tot_gross, na.rm = T), 1)
      kpi_card("Best Gross:", value, class = 'kpi-best')
    })

    output$best_net <- shiny::renderUI({
      df <- filtered()
      shiny::req(nrow(df) > 0)
      value <- round(min(df$tot_net, na.rm = T), 1)
      kpi_card("Best Net:", value, class = 'kpi-best')
    })

    output$best_index <- shiny::renderUI({
      df <- filtered()
      value <- round(min(df$handicap_index, na.rm = T), 1)
      kpi_card("Best H.I.:", value, class = 'kpi-best')
    })

    # output$best_fir <- shiny::renderUI({
    #   df <- filtered()
    #   value <- round(max(df$fir, na.rm = T), 1)
    #   kpi_card("Best FIR %:", value)
    # })
    # 
    # output$best_gir <- shiny::renderUI({
    #   df <- filtered()
    #   value <- round(max(df$gir, na.rm = T), 1)
    #   kpi_card("Best GIR %:", value)
    # })
    # 
    # output$best_putts <- shiny::renderUI({
    #   df <- filtered()
    #   value <- round(min(df$tot_putts, na.rm = T), 1)
    #   kpi_card(strong("Min Tot. Putts:"), value)
    # })
    
    ts_map <- list(
      "Gross Score"     = list(col = "tot_gross",       label = "Gross Score"),
      "Net Score"       = list(col = "tot_net",         label = "Net Score"),
      "Handicap Index"  = list(col = "handicap_index",  label = "Handicap Index"),
      "Skill Estimate"   = list(col = "skill_est",     label = "Skill Estimate")
    )
    
    # force ts rebuild
    output$ts_plot_container <- shiny::renderUI({
      input$ts_choice
      shiny::div(
        style = "height: 40vh; min-height: 250px;",
        apexchartOutput(session$ns("ts_plot"))
      )
    })
    
    # Time series plot
    output$ts_plot <- renderApexchart({
      shiny::req(input$ts_choice)
      spec <- ts_map[[input$ts_choice]]
      # message("metric = ", spec$col, " | label = ", spec$label)
      make_ts_plot(filtered(), metric = spec$col, label = spec$label)
    })
  })
}


