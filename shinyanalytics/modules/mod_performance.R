# mod_performance_ui <- function(id) {
#   ns <- shiny::NS(id)
#   
#   shiny::tabPanel(
#     title = "Performance",
# 
#     icon = 'chart-bar',
# 
#     shiny::sidebarLayout(
# 
#       shiny::sidebarPanel(
#         width = 3,
# 
#         # Metric choice
#         shiny::selectInput(
#           inputId = ns("metric_choice"),
#           label   = "Metric (Choose One):",
#           choices = c(
#             "FIR %",
#             "GIR %",
#             "Putts per Round",
#             "Chips per Round",
#             "Putts + Chips per Round",
#             "Up & Down %",
#             "Penalties per Round",
#             "FIR % by Tee Club",
#             "GIR % by Tee Club"
#           ),
#           selected = "GIR %",
#           multiple = F
#         ),
# 
#         shiny::hr(),
# 
#         # X-axis selector (date vs metric)
#         shiny::selectInput(
#           inputId = ns("x_choice"),
#           label   = "X-axis:",
#           choices = c("Date", "Metric"),
#           selected = "Metric"
#         ),
# 
#         # Facet selector
#         shiny::selectInput(
#           inputId = ns("facet_choice"),
#           label   = "Facet by (Choose One):",
#           choices = c("None", "Course", "Tee Club"),
#           selected = "None"
#         ),
# 
#         # Smoothing toggle
#         shiny::checkboxInput(
#           inputId = ns("smooth"),
#           label   = "Add LM Smoothing",
#           value   = T
#         ),
# 
#         # Flip axes toggle
#         shiny::checkboxInput(
#           inputId = ns("flip_axes"),
#           label   = "Flip Axes",
#           value   = F
#         )
#       ),
# 
#       shiny::mainPanel(
#         width = 9,
#         class = 'main-panel',
# 
#         shiny::div(
#           style = "display: flex; flex-wrap: wrap; gap: 20px;",
# 
#           shiny::div(shiny::uiOutput(ns("kpi_fir"))), # removed style = "width: 200px;"
#           shiny::div(shiny::uiOutput(ns("best_fir"))),
#           shiny::div(shiny::uiOutput(ns("kpi_gir"))),
#           shiny::div(shiny::uiOutput(ns("best_gir"))),
#           shiny::div(shiny::uiOutput(ns("kpi_updown"))),
#           shiny::div(shiny::uiOutput(ns("kpi_putts"))),
#           shiny::div(shiny::uiOutput(ns("best_putts"))),
#           shiny::div(shiny::uiOutput(ns("kpi_penalties")))
#         ),
# 
#         shiny::hr(),
# 
#         shiny::div(
#           # style = "height: 40vh; min-height: 250px;",
#           shiny::plotOutput(
#             outputId = ns("metric_plot")#,
#             # height = "auto"
#           )
#         )
#       )
#     )
#   )
# }
# 
# mod_performance_server <- function(id, data_r) {
#   shiny::moduleServer(id, function(input, output, session) {
#     ns <- session$ns
#     
#     performance_round_df <- shiny::reactive({
#       df <- data_r()
#       
#       
#       df <- df |> 
#         dplyr::mutate(club = factor(club, levels = c("LW", "SW", "GW", "PW", "9", "8", "7", "6", "5", "4", "3W", "D"))) |> 
#         dplyr::group_by(date, course_name, tees, handicap_index) |> 
#         dplyr::summarize(fir = dplyr::first(fir),
#                          gir = dplyr::first(gir),
#                          updown = dplyr::first(updown),
#                          tot_putts = dplyr::first(tot_putts),
#                          tot_chips = dplyr::first(tot_chips),
#                          tot_putts_and_chips = dplyr::first(tot_putts_and_chips),
#                          tot_penalties = dplyr::first(tot_penalties),
#                          tot_gross = dplyr::first(tot_gross),
#                          .groups = 'drop')
#     })
#     
#     performance_club_df <- shiny::reactive({
#       df <- data_r()
#       
#       df <- df |> 
#         dplyr::mutate(club = factor(club, levels = c("LW", "SW", "GW", "PW", "9", "8", "7", "6", "5", "4", "3W", "D"))) |> 
#         dplyr::filter(!is.na(tee_club)) |> 
#         dplyr::group_by(date, course_name, tees, handicap_index, par, tee_club) |> 
#         dplyr::summarize(
#           tee_club_fir = mean(tee_club_fir[par %in% c(4, 5)], na.rm = T)*100,
#           tee_club_gir = mean(tee_club_gir, na.rm = T)*100,
#           tot_gross = dplyr::first(tot_gross),
#           .groups = 'drop'
#         ) |> 
#         dplyr::ungroup()
#     })
#     
#     output$kpi_fir <- shiny::renderUI({
#       df <- performance_round_df()
#       shiny::req(nrow(df) > 0)
#       value <- round(mean(df$fir, na.rm = T), 1)
#       kpi_card("Avg. FIR %:", value)
#     })
#     
#     output$best_fir <- shiny::renderUI({
#       df <- performance_round_df()
#       value <- round(max(df$fir, na.rm = T), 1)
#       kpi_card("Best FIR %:", value, class = 'kpi-best')
#     })
#     
#     output$kpi_gir <- shiny::renderUI({
#       df <- performance_round_df()
#       shiny::req(nrow(df) > 0)
#       value <- round(mean(df$gir, na.rm = T), 1)
#       kpi_card("Avg. GIR %:", value)
#     })
#     
#     output$best_gir <- shiny::renderUI({
#       df <- performance_round_df()
#       value <- round(max(df$gir, na.rm = T), 1)
#       kpi_card("Best GIR %:", value, class = 'kpi-best')
#     })
#     
#     output$kpi_updown <- shiny::renderUI({
#       df <- performance_round_df()
#       shiny::req(nrow(df) > 0)
#       value <- round(mean(df$updown, na.rm = T), 1)
#       kpi_card("Avg.\nUp & Down %:", value)
#     })
#     
#     output$kpi_putts <- shiny::renderUI({
#       df <- performance_round_df()
#       shiny::req(nrow(df) > 0)
#       value <- round(mean(df$tot_putts, na.rm = T), 1)
#       kpi_card("Avg.\nTotal Putts:", value)
#     })
#     
#     output$best_putts <- shiny::renderUI({
#       df <- performance_round_df()
#       value <- round(min(df$tot_putts, na.rm = T), 1)
#       kpi_card(strong("Min Tot. Putts:"), value, class = 'kpi-best')
#     })
#     
#     output$kpi_penalties <- shiny::renderUI({
#       df <- performance_round_df()
#       shiny::req(nrow(df) > 0)
#       value <- round(mean(df$tot_penalties, na.rm = T), 1)
#       kpi_card("Avg.\nTotal Penalties:", value)
#     })
#     
#     output$metric_plot <- shiny::renderPlot(height = function() 300, res = 96, {
#       df_round <- performance_round_df()
#       df_club <- performance_club_df()
# 
#       req(df_round)
#       req(df_club)
# 
#       make_metric_plot(
#         df_round = df_round,
#         df_club = df_club,
#         metric = input$metric_choice,
#         facet = input$facet_choice,
#         smooth = input$smooth,
#         flip = input$flip_axes,
#         x_choice = input$x_choice
#       )
#     })
#     
#   })
# }

mod_performance_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::sidebarLayout(
    position = "left",
    
    shiny::sidebarPanel(
      width = 3,
      
      shiny::sliderInput(
        ns("window_start"),
        label = "Recency Window Start (days ago)",
        min = 0,
        max = 365,
        value = 0,
        step = 1
      ),
      
      shiny::sliderInput(
        ns("window_end"),
        label = "Recency Window End (days ago)",
        min = 1,
        max = 365,
        value = 90,
        step = 1
      ),
      
      shiny::selectInput(
        inputId = ns("metric_choice"),
        label   = "Metric (Choose One):",
        choices = list(
          "Ball Striking" = c(
            "FIR %",
            "GIR %",
            "FIR % by Tee Club",
            "GIR % by Tee Club"
          ),
          "Short Game" = c(
            "Up & Down %",
            "Putts per Round",
            "Chips per Round",
            "Putts + Chips per Round"
          ),
          "Scoring" = c(
            "Penalties per Round",
            "Gross Score"
          )
        ),
        selected = "GIR %"
      ),
      
      shiny::selectInput(
        inputId = ns("x_choice"),
        label   = "X-axis:",
        choices = c("Date", "Metric"),
        selected = "Date"
      ),
      
      shiny::selectInput(
        inputId = ns("facet_choice"),
        label   = "Facet by:",
        choices = c("None", "Course", "Tee Club"),
        selected = "None"
      ),
      
      shiny::checkboxInput(
        inputId = ns("smooth"),
        label   = "Add LM Smoothing",
        value   = TRUE
      ),
      
      shiny::checkboxInput(
        inputId = ns("flip_axes"),
        label   = "Flip Axes",
        value   = FALSE
      )
    ),
    
    shiny::mainPanel(
      width = 9,
      style = "padding: 10px;",
      
      shiny::uiOutput(ns("metric_description")),
      
      shiny::div(
      style = "width: 100%;",
        # shiny::div(style = "border: 3px solid red; height: 600px;", "TEST BLOCK"),
        shiny::plotOutput(ns("metric_plot"))#, height = "500px")
      )
    )
  )
}

mod_performance_server <- function(id, data_r, equipment = NULL) {
  
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    metric_description <- shiny::reactive({
      m <- input$metric_choice
      
      if (is.null(m)) {
        return("");
      } else {
        if (m == "FIR %") {
          return("Percentage of tee shots on par‑4/5 holes\nthat end in the fairway");
        } else if (m == "GIR %") {
          return("Percentage of holes where the green\nis reached in regulation");
        } else if (m == "Tee Club FIR %") {
          return("Fairway percentage broken down by\nthe club used off the tee");
        } else if (m == "Tee Club GIR %") {
          return("Green‑in‑regulation percentage\ngrouped by tee club selection");
        } else if (m == "Up & Down %") {
          return("Percentage of missed‑green holes\nwhere par is still made");
        } else if (m == "Putts per Round") {
          return("Total number of putts taken per round");
        } else if (m == "Chips per Round") {
          return("Total number of chip shots taken per round");
        } else if (m == "Putts + Chips per Round") {
          return("Combined short‑game workload per round");
        } else if (m == "Penalties per Round") {
          return("Average number of penalty strokes incurred\nper round");
        } else if (m == "Gross Score") {
          return("Total strokes taken per round");
        } else if (m == "Dispersion") {
          return("Spread and directional bias of shot outcomes\nrelative to target line");
        } else if (m == "Club Trends") {
          return("Performance patterns and tendencies grouped by club");
        } else if (m == "Equipment") {
          return("Performance characteristics associated with selected bag setup");
        } else if (m == "Strokes Gained") {
          return("Relative performance versus benchmark skill levels across shot types");
        } else {
          return("");
        }
      }
    })
    
    output$metric_description <- shiny::renderUI({
      desc <- metric_description()
      
      shiny::HTML(
        paste0(
          "<div style='font-weight:bold; font-size:16px;'>",
          desc,
          "</div>"
        )
      )
    })
    
    performance_round_df <- shiny::reactive({
      df <- data_r()
      
      if (is.null(df)) {
        return(NULL)
      }
      
      if (!("date" %in% names(df))) {
        return(NULL)
      }
      
      df <- df |>
        dplyr::group_by(
          .data$date,
          .data$course_name,
          .data$tees,
          .data$handicap_index
        ) |>
        dplyr::summarize(
          fir                 = dplyr::first(.data$fir),
          gir                 = dplyr::first(.data$gir),
          updown              = dplyr::first(.data$updown),
          tot_putts           = dplyr::first(.data$tot_putts),
          tot_chips           = dplyr::first(.data$tot_chips),
          tot_putts_and_chips = dplyr::first(.data$tot_putts_and_chips),
          tot_penalties       = dplyr::first(.data$tot_penalties),
          tot_gross           = dplyr::first(.data$tot_gross),
          .groups             = "drop"
        )
      
      df <- df |> 
        dplyr::filter(is.finite(handicap_index))
      
      return(df)
    })
    
    performance_club_df <- shiny::reactive({
      df <- data_r()
      
      if (is.null(df)) {
        return(NULL)
      }
      
      if (!("tee_club" %in% names(df))) {
        return(NULL)
      }
      
      df <- df |>
        dplyr::filter(!is.na(.data$tee_club)) |>
        dplyr::group_by(
          .data$date,
          .data$course_name,
          .data$tees,
          .data$handicap_index,
          .data$par,
          .data$tee_club
        ) |>
        dplyr::summarize(
          tee_club_fir = round(mean(.data$tee_club_fir[.data$par %in% c(4, 5)], na.rm = TRUE) * 100,1),
          tee_club_gir = round(mean(.data$tee_club_gir, na.rm = TRUE) * 100,1),
          tot_gross    = dplyr::first(.data$tot_gross),
          .groups      = "drop"
        )
      
      df <- df |> 
        dplyr::filter(is.finite(handicap_index) | is.finite(tee_club_gir))
      
      return(df)
    })
    
    filter_window <- shiny::reactive({
      df <- performance_round_df()
      
      if (is.null(df) || nrow(df) == 0) {
        return(df)
      }
      
      df <- df |> 
        dplyr::filter(!is.na(date), is.finite(date))
      
      window_start <- input$window_start
      window_end <- input$window_end
      
      if (is.null(window_start) || is.null(window_end)) {
        return(df)
      }
      
      max_date <- max(df$date, na.rm = TRUE)

      df |> dplyr::filter(date >= max_date - window_end,
                          date <= max_date - window_start,
                          !is.na(date),
                          is.finite(date))
    })
    
    output$metric_plot <- shiny::renderPlot({
      
      df_round <- filter_window()
      df_club  <- performance_club_df()
      m        <- input$metric_choice

      if (is.null(df_round)) {
        return(ggplot2::ggplot() +
                 ggplot2::annotate("text", x = 0.5, y = 0.5,
                                   label = "No data available.", size = 6) +
                 ggplot2::theme_bw())
      }

      if (nrow(df_round) == 0) {
        return(ggplot2::ggplot() +
                 ggplot2::annotate("text", x = 0.5, y = 0.5,
                                   label = "No data in this window.", size = 6) +
                 ggplot2::theme_bw())
      }

      if (nrow(df_round) < 2) {
        return(ggplot2::ggplot() +
                 ggplot2::annotate("text", x = 0.5, y = 0.5,
                                   label = "Not enough data in this window.", size = 6) +
                 ggplot2::theme_bw())
      }

      make_metric_plot(
        df_round = df_round,
        df_club  = df_club,
        metric   = m,
        facet    = input$facet_choice,
        smooth   = input$smooth,
        flip     = input$flip_axes,
        x_choice = input$x_choice,
        window_df = list(
          window_start = input$window_start,
          window_end = input$window_end
        )
      )
    })
    
  })
}



