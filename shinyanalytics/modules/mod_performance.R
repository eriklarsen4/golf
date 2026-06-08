mod_performance_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tabPanel(
    title = "Performance",
    
    icon = 'chart-bar',
    
    shiny::sidebarLayout(
      
      shiny::sidebarPanel(
        width = 3,
        
        # Metric choice
        shiny::selectInput(
          inputId = ns("metric_choice"),
          label   = "Metric (Choose One):",
          choices = c(
            "FIR %",
            "GIR %",
            "Putts per Round",
            "Chips per Round",
            "Putts + Chips per Round",
            "Up & Down %",
            "Penalties per Round",
            "FIR % by Tee Club",
            "GIR % by Tee Club"
          ),
          selected = "GIR %",
          multiple = F
        ),
        
        shiny::hr(),
        
        # X-axis selector (date vs metric)
        shiny::selectInput(
          inputId = ns("x_choice"),
          label   = "X-axis:",
          choices = c("Date", "Metric"),
          selected = "Metric"
        ),
        
        # Facet selector
        shiny::selectInput(
          inputId = ns("facet_choice"),
          label   = "Facet by (Choose One):",
          choices = c("None", "Course", "Tee Club"),
          selected = "None"
        ),
        
        # Smoothing toggle
        shiny::checkboxInput(
          inputId = ns("smooth"),
          label   = "Add LM Smoothing",
          value   = T
        ),
        
        # Flip axes toggle
        shiny::checkboxInput(
          inputId = ns("flip_axes"),
          label   = "Flip Axes",
          value   = F
        )
      ),
      
      shiny::mainPanel(
        width = 9,
        class = 'main-panel',
        
        shiny::div(
          style = "display: flex; flex-wrap: wrap; gap: 20px;",
          
          shiny::div(style = "width: 200px;", shiny::uiOutput(ns("kpi_fir"))),
          shiny::div(style = "width: 200px;", shiny::uiOutput(ns("kpi_gir"))),
          shiny::div(style = "width: 200px;", shiny::uiOutput(ns("kpi_updown"))),
          shiny::div(style = "width: 200px;", shiny::uiOutput(ns("kpi_putts"))),
          shiny::div(style = "width: 200px;", shiny::uiOutput(ns("kpi_penalties")))
        ),
        
        shiny::hr(),
        
        shiny::plotOutput(
          outputId = ns("metric_plot"),
          height = "auto",
          style = "height: 40vh; min-height: 250px;"
        )
      )
    )
  )
}


mod_performance_server <- function(id, data_r) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    performance_round_df <- shiny::reactive({
      df <- data_r()
      
      
      df <- df |> 
        dplyr::mutate(club = factor(club, levels = c("LW", "SW", "GW", "PW", "9", "8", "7", "6", "5", "4", "3W", "D"))) |> 
        dplyr::group_by(date, course_name, tees, handicap_index) |> 
        dplyr::summarize(fir = dplyr::first(fir),
                         gir = dplyr::first(gir),
                         updown = dplyr::first(updown),
                         tot_putts = dplyr::first(tot_putts),
                         tot_chips = dplyr::first(tot_chips),
                         tot_putts_and_chips = dplyr::first(tot_putts_and_chips),
                         tot_penalties = dplyr::first(tot_penalties),
                         tot_gross = dplyr::first(tot_gross),
                         .groups = 'drop')
    })
    
    performance_club_df <- shiny::reactive({
      df <- data_r()
      
      df <- df |> 
        dplyr::mutate(club = factor(club, levels = c("LW", "SW", "GW", "PW", "9", "8", "7", "6", "5", "4", "3W", "D"))) |> 
        dplyr::filter(!is.na(tee_club)) |> 
        dplyr::group_by(date, course_name, tees, handicap_index, par, tee_club) |> 
        dplyr::summarize(
          tee_club_fir = mean(tee_club_fir[par %in% c(4, 5)], na.rm = T)*100,
          tee_club_gir = mean(tee_club_gir, na.rm = T)*100,
          tot_gross = dplyr::first(tot_gross),
          .groups = 'drop'
        ) |> 
        dplyr::ungroup()
    })
    
    output$kpi_fir <- shiny::renderUI({
      df <- performance_round_df()
      shiny::req(nrow(df) > 0)
      value <- round(mean(df$fir, na.rm = T), 1)
      kpi_card("Avg. FIR %:", value)
    })
    
    output$kpi_gir <- shiny::renderUI({
      df <- performance_round_df()
      shiny::req(nrow(df) > 0)
      value <- round(mean(df$gir, na.rm = T), 1)
      kpi_card("Avg. GIR %:", value)
    })
    
    output$kpi_updown <- shiny::renderUI({
      df <- performance_round_df()
      shiny::req(nrow(df) > 0)
      value <- round(mean(df$updown, na.rm = T), 1)
      kpi_card("Avg. Up & Down %:", value)
    })
    
    output$kpi_putts <- shiny::renderUI({
      df <- performance_round_df()
      shiny::req(nrow(df) > 0)
      value <- round(mean(df$tot_putts, na.rm = T), 1)
      kpi_card("Avg. Total Putts:", value)
    })
    
    output$kpi_penalties <- shiny::renderUI({
      df <- performance_round_df()
      shiny::req(nrow(df) > 0)
      value <- round(mean(df$tot_penalties, na.rm = T), 1)
      kpi_card("Avg. Total Penalties:", value)
    })
    
    output$metric_plot <- shiny::renderPlot(height = function() 300, res = 96, {
      df_round <- performance_round_df()
      df_club <- performance_club_df()

      req(df_round)
      req(df_club)

      make_metric_plot(
        df_round = df_round,
        df_club = df_club,
        metric = input$metric_choice,
        facet = input$facet_choice,
        smooth = input$smooth,
        flip = input$flip_axes,
        x_choice = input$x_choice
      )
    })
    
  })
}


