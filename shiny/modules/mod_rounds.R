mod_rounds_ui <- function(id) {
  ns <- shiny::NS(id)
  
  bslib::layout_sidebar(
    sidebar = shiny::tagList(
      shiny::selectInput(
        ns("round_id"),
        "Round",
        choices = NULL
      ),
      shiny::selectInput(
        ns("hole"),
        "Hole",
        choices = NULL
      ),
      shiny::selectInput(
        ns("club_filter"),
        "Club Filter",
        choices = c("All"),
        selected = "All"
      ),
      shiny::selectInput(
        ns("lie_filter"),
        "Lie Filter",
        choices = c("All"),
        selected = "All"
      ),
      shiny::checkboxInput(
        ns("include_putts"),
        "Include Putts",
        value = TRUE
      ),
      shiny::checkboxInput(
        ns("include_penalties"),
        "Include Penalties",
        value = TRUE
      ),
      shiny::downloadButton(
        ns("download_scorecard"),
        "Download Scorecard"
      )
    ),
    
    bslib::card(
      shiny::h4("Round Summary"),
      shiny::tableOutput(ns("round_summary"))
    ),
    
    bslib::card(
      shiny::h4("Round Metrics"),
      shiny::tableOutput(ns("round_metrics"))
    ),
    
    bslib::card(
      shiny::h4("Hole Summary"),
      shiny::tableOutput(ns("hole_summary"))
    ),
    
    bslib::card(
      shiny::h4("Hole Metrics"),
      shiny::tableOutput(ns("hole_metrics"))
    ),
    
    bslib::card(
      shiny::h4("Club Metrics (This Round)"),
      shiny::tableOutput(ns("club_metrics"))
    ),
    
    bslib::card(
      shiny::h4("Lie Metrics (This Round)"),
      shiny::tableOutput(ns("lie_metrics"))
    ),
    
    bslib::card(
      shiny::h4("Shot-Type Metrics (This Round)"),
      shiny::tableOutput(ns("shottype_metrics"))
    ),
    
    bslib::card(
      shiny::h4("Shot List"),
      shiny::tableOutput(ns("shot_table"))
    )
  )
}

mod_rounds_server <- function(id, data) {
  shiny::moduleServer(id, function(input, output, session) {
    
    # initialize round selector
    shiny::observe({
      rounds <- sort(unique(data$rounds$round_id))
      shiny::updateSelectInput(session, "round_id", choices = rounds)
    })
    
    # update hole selector when round changes
    shiny::observeEvent(input$round_id, {
      holes <- data$rounds |>
        dplyr::filter(round_id == input$round_id) |>
        dplyr::pull(hole)
      
      shiny::updateSelectInput(session, "hole", choices = holes)
    })
    
    # update club and lie filters based on selected round
    shiny::observeEvent(input$round_id, {
      df <- data$strokes |>
        dplyr::filter(round_id == input$round_id)
      
      shiny::updateSelectInput(
        session,
        "club_filter",
        choices = c("All", sort(unique(df$club)))
      )
      
      shiny::updateSelectInput(
        session,
        "lie_filter",
        choices = c("All", sort(unique(df$lie)))
      )
    })
    
    # filtered strokes for selected round + hole
    filtered_strokes <- shiny::reactive({
      df <- data$strokes |>
        dplyr::filter(
          round_id == input$round_id,
          hole == input$hole
        )
      
      if (input$club_filter != "All") {
        df <- df |> dplyr::filter(club == input$club_filter)
      }
      
      if (input$lie_filter != "All") {
        df <- df |> dplyr::filter(lie == input$lie_filter)
      }
      
      if (!input$include_putts) {
        df <- df |> dplyr::filter(shot_type != "putt")
      }
      
      if (!input$include_penalties) {
        df <- df |> dplyr::filter(penalty == 0)
      }
      
      df
    })
    
    # ---- ROUND SUMMARY ----
    output$round_summary <- shiny::renderTable({
      data$rounds |>
        dplyr::filter(round_id == input$round_id) |>
        dplyr::select(
          hole, par, score, fir, gir, putts, penalties
        )
    })
    
    # ---- ROUND METRICS ----
    output$round_metrics <- shiny::renderTable({
      df <- data$rounds |>
        dplyr::filter(round_id == input$round_id)
      
      tibble::tibble(
        Metric = c(
          "Total Score",
          "FIR %",
          "GIR %",
          "Scrambling %",
          "Total Putts",
          "Penalties",
          "Par-3 Avg",
          "Par-4 Avg",
          "Par-5 Avg"
        ),
        Value = c(
          sum(df$score, na.rm = TRUE),
          round(mean(df$fir, na.rm = TRUE) * 100, 1),
          round(mean(df$gir, na.rm = TRUE) * 100, 1),
          round(mean(df$scrambling, na.rm = TRUE) * 100, 1),
          sum(df$putts, na.rm = TRUE),
          sum(df$penalties, na.rm = TRUE),
          round(mean(df$score[df$par == 3], na.rm = TRUE), 2),
          round(mean(df$score[df$par == 4], na.rm = TRUE), 2),
          round(mean(df$score[df$par == 5], na.rm = TRUE), 2)
        )
      )
    })
    
    # ---- HOLE SUMMARY ----
    output$hole_summary <- shiny::renderTable({
      data$rounds |>
        dplyr::filter(
          round_id == input$round_id,
          hole == input$hole
        ) |>
        dplyr::select(
          hole, par, score, fir, gir, putts, penalties
        )
    })
    
    # ---- HOLE METRICS ----
    output$hole_metrics <- shiny::renderTable({
      df <- data$rounds |>
        dplyr::filter(
          round_id == input$round_id,
          hole == input$hole
        )
      
      tibble::tibble(
        Metric = c(
          "Strokes",
          "FIR",
          "GIR",
          "Putts",
          "Penalties"
        ),
        Value = c(
          df$score,
          df$fir,
          df$gir,
          df$putts,
          df$penalties
        )
      )
    })
    
    # ---- CLUB METRICS ----
    output$club_metrics <- shiny::renderTable({
      df <- data$strokes |>
        dplyr::filter(round_id == input$round_id)
      
      df |>
        dplyr::group_by(club) |>
        dplyr::summarise(
          shots = dplyr::n(),
          avg_yds_to_target = mean(yds_to_target, na.rm = TRUE),
          avg_yds_traveled = mean(yds_traveled, na.rm = TRUE),
          penalties = sum(penalty, na.rm = TRUE)
        )
    })
    
    # ---- LIE METRICS ----
    output$lie_metrics <- shiny::renderTable({
      df <- data$strokes |>
        dplyr::filter(round_id == input$round_id)
      
      df |>
        dplyr::group_by(lie) |>
        dplyr::summarise(
          shots = dplyr::n(),
          avg_yds_to_target = mean(yds_to_target, na.rm = TRUE),
          avg_yds_traveled = mean(yds_traveled, na.rm = TRUE),
          penalties = sum(penalty, na.rm = TRUE)
        )
    })
    
    # ---- SHOT-TYPE METRICS ----
    output$shottype_metrics <- shiny::renderTable({
      df <- data$strokes |>
        dplyr::filter(round_id == input$round_id)
      
      df |>
        dplyr::group_by(shot_type) |>
        dplyr::summarise(
          shots = dplyr::n(),
          avg_yds_to_target = mean(yds_to_target, na.rm = TRUE),
          avg_yds_traveled = mean(yds_traveled, na.rm = TRUE),
          penalties = sum(penalty, na.rm = TRUE)
        )
    })
    
    # ---- SHOT LIST ----
    output$shot_table <- shiny::renderTable({
      filtered_strokes() |>
        dplyr::select(
          shot_number,
          club,
          lie,
          yds_to_target,
          yds_traveled,
          miss_direction,
          result,
          strokes_gained
        )
    })
    
    # ---- SCORECARD DOWNLOAD ----
    output$download_scorecard <- shiny::downloadHandler(
      filename = function() {
        paste0("scorecard_round_", input$round_id, ".csv")
      },
      content = function(file) {
        df <- data$rounds |>
          dplyr::filter(round_id == input$round_id)
        
        utils::write.csv(df, file, row.names = FALSE)
      }
    )
  })
}

