mod_rounds_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tabPanel(
    title = "Rounds",
    
    shiny::sidebarLayout(
      
      shiny::sidebarPanel(
        width = 3,
        
        shiny::selectInput(
          inputId = ns("date"),
          label   = "Round",
          choices = NULL,
          selected = NULL,
          multiple = F
        ),
        
        shiny::selectInput(
          inputId = ns("hole"),
          label   = "Hole",
          choices = NULL,
          selected = NULL,
          multiple = F
        ),
        
        shiny::selectInput(
          inputId = ns("club_filter"),
          label   = "Club Filter",
          choices = c("All", "P", "LW", "SW", "GW", "PW", "9", "8", "7", "6", "5", "4", "3W", "D"),
          selected = "All",
          multiple = T
        ),
        
        shiny::selectInput(
          inputId = ns("lie_filter"),
          label   = "Lie Filter",
          choices = c("All", "tee", "fairway", "rough", "sand"),
          selected = "All",
          multiple = T
        ),
        
        shiny::checkboxInput(
          inputId = ns("include_putts"),
          label   = "Include Putts",
          value   = T
        ),
        
        shiny::checkboxInput(
          inputId = ns("include_penalties"),
          label   = "Include Penalties",
          value   = T
        ),
        
        shiny::downloadButton(
          outputId = ns("download_scorecard"),
          label    = "Download Scorecard"
        )
      ),
      
      shiny::mainPanel(
        width = 9,
        
        div(class = 'table_scroll',
            style = 'overflow-x: auto; width: 100%',
            shiny::h4(strong("Round Summary")),
            shiny::tableOutput(ns("round_summary")),
            
            shiny::h4(strong("Round Metrics")),
            shiny::tableOutput(ns("round_metrics")),
            
            shiny::h4(strong("Hole Summary")),
            shiny::tableOutput(ns("hole_summary")),
            
            shiny::h4(strong("Hole Metrics")),
            shiny::tableOutput(ns("hole_metrics")),
            
            shiny::h4(strong("Club Metrics (This Round)")),
            shiny::tableOutput(ns("club_metrics")),
            
            shiny::h4(strong("Lie Metrics (This Round)")),
            shiny::tableOutput(ns("lie_metrics")),
            
            shiny::h4(strong("Shot-Type Metrics (This Round)")),
            shiny::tableOutput(ns("shottype_metrics")),
            
            shiny::h4(strong("Shot List")),
            shiny::tableOutput(ns("shot_table"))
        )
      )
    )
  )
}


mod_rounds_server <- function(id, data_r) {
  shiny::moduleServer(id, function(input, output, session) {
    
    summarize_round <- function(df) {
      df |> 
        dplyr::select(c(GHIN:tees), dplyr::contains("tot_"), fir, gir, updown) |> 
        dplyr::distinct()
    }
    
    summarize_metrics <- function(df) {
      df |> 
        dplyr::filter(!is.na(stroke)) |> 
        dplyr::mutate(GIR_putts = dplyr::case_when(GIR == 1 ~ putts,
                                                   TRUE ~ NA_real_)) |> 
        dplyr::mutate(
          approach_shots = dplyr::case_when(
            par == 3 & stroke == 1 ~ 1,
            par > 3 &
              shot_type == "full" &
              yds_to_target > 75 &
              club != "D" &
              !grepl(lie, pattern = "sand|tee") ~ 1,
            TRUE ~ 0
          ),
          approach_club = dplyr::case_when(approach_shots == 1 ~ club,
                                           TRUE ~ NA)
        ) |> 
        dplyr::mutate(tee_shot_distance = dplyr::case_when(stroke == 1 & par > 3 ~ yds_traveled,
                                                           TRUE ~ NA_real_),
                      approach_distance = dplyr::case_when(approach_shots == 1 ~ yds_traveled,
                                                           TRUE ~ NA_real_)) |> 
        dplyr::group_by(GHIN, course_name, tees, date) |> 
        dplyr::summarize(dplyr::across(c(tee_shot_distance, approach_distance, GIR_putts, putts), mean, na.rm = T, .names = 'avg_{col}')) |> 
        dplyr::arrange(date)
      
    }
    
    summarize_hole <- function(df){
      df |> 
        dplyr::select(c(GHIN:hole), par, gross, net, FIR, GIR, putts, penalties) |> 
        dplyr::distinct() |> 
        dplyr::group_by(course_name, hole) |> 
        dplyr::summarize(times_played = dplyr::n(), dplyr::across(c(par:penalties), mean, na.rm = T, .names = 'avg_{col}')) |> 
        dplyr::mutate(dplyr::across(c(avg_par:avg_penalties), ~round(.x,1))) |> 
        dplyr::distinct() |> 
        dplyr::rename(par = avg_par)
    }
    
    summarize_hole_metrics <- function(df) {
      df |> 
        dplyr::mutate(on_target = dplyr::case_when(on_target == "yes" ~ 1,
                                                   on_target == 'no' ~ 0,
                                                   TRUE ~ NA_real_)) |> 
        dplyr::select(c(GHIN:hole), stroke, GIR, FIR, chips, putts, scrambles, on_target, dplyr::starts_with("is_")) |> 
        dplyr::group_by(course_name, hole, tees, stroke) |> 
        dplyr::mutate(non_putts_on_target = sum(on_target, na.rm = T)) |> 
        dplyr::select(-stroke, -on_target) |> 
        dplyr::distinct() |> 
        dplyr::group_by(course_name, tees, hole) |> 
        dplyr::summarize(dplyr::across(c(GIR:non_putts_on_target), list(mean = mean, tot = sum), na.rm = T)) %>%
        dplyr::rename_with(.cols = c('GIR_mean', 'FIR_mean', 'scrambles_mean', 'non_putts_on_target_mean', dplyr::contains("is_")), ~gsub(.x, pattern = '_mean', replacement = ' %')) %>%
        dplyr::rename_with(.cols = dplyr::matches("is_(gross|net)_(birdie|par|bogey_|bogey|bogey_worse|bogey_worse_|eagle_better|eagle_better_)_tot"), 
                           ~gsub(.x, pattern = '(is)(\\_)(gross|net)(\\_)(birdie|par|bogey_|bogey|bogey_worse|bogey_worse_|eagle_better|eagle_better_)_tot', 
                                 replacement = '\\3 \\5')) |> 
        
        dplyr::rename_with(.cols = dplyr::matches("is_(gross|net)_(birdie|par|bogey_|bogey|bogey_worse|bogey_worse_|eagle_better|eagle_better_) %"), 
                           ~gsub(.x, pattern = '(is)(\\_)(gross|net)(\\_)(birdie|par|bogey_|bogey|bogey_worse|bogey_worse_|eagle_better|eagle_better_)( %)', 
                                 replacement = '\\3 \\5\\6')) |> 
        
        dplyr::rename_with(.cols = dplyr::matches("tot"), ~gsub(.x, pattern = '_tot', replacement = '')) |> 
        dplyr::rename_with(.cols = dplyr::matches("_(better|worse)"), ~gsub(.x, pattern = '_', replacement = ' or ')) |> 
        dplyr::rename_with(.cols = dplyr::ends_with("bogey"), ~gsub(.x, pattern = 'bogey', replacement = 'bogies')) |> 
        dplyr::rename_with(.cols = dplyr::matches("(birdie$|par$)"), ~gsub(.x, pattern = '(^.+?)(birdie$|par$)', replacement = '\\1\\2s')) %>%
        dplyr::mutate(dplyr::across(dplyr::contains("%"), ~.x*100)) |> 
        dplyr::distinct()
    }
    
    summarize_club <- function(df) {
      club_levels <- c("LW", "SW", "GW", "PW", "9", "8", "7", "6", "5", "4", "3W", "D")
      
      df |>
        dplyr::filter(!is.na(stroke), 
                      !is.na(club), 
                      club != 'P',
                      shot_type %in% c('full', 'tee', 'chip')) |> 
        dplyr::mutate(distance_loss = yds_to_target - yds_traveled,
                      on_target = dplyr::case_when(on_target == 'yes' ~ 1, on_target == 'no' ~ 0, TRUE ~ NA_real_),
                      club = factor(club, levels = club_levels)
        ) |> 
        dplyr::select(c(GHIN:hole, stroke, club, yds_traveled, distance_loss, miss_direction, on_target)) |> 
        dplyr::group_by(club) |> 
        dplyr::summarize(strokes = dplyr::n(),
                         median_yds_traveled = round(stats::median(yds_traveled, na.rm = T),1),
                         accuracy_pct = round(mean(on_target, na.rm = T)*100, 1),
                         median_yds_lost = round(stats::median(distance_loss, na.rm = T), 1)) |> 
        dplyr::left_join(
          
          df |> 
            dplyr::filter(!is.na(stroke), 
                          !is.na(club), 
                          club != 'P',
                          shot_type %in% c('full', 'tee', 'chip')) |> 
            dplyr::mutate(distance_loss = yds_to_target - yds_traveled,
                          on_target = dplyr::case_when(on_target == 'yes' ~ 1, on_target == 'no' ~ 0, TRUE ~ NA_real_),
                          club = factor(club, levels = club_levels)
            ) |> 
            dplyr::select(c(GHIN:hole, stroke, club, yds_traveled, distance_loss, miss_direction, on_target)) |> 
            dplyr::group_by(club) |> 
            dplyr::count(club, miss_direction) |> 
            dplyr::group_by(club) |> 
            dplyr::arrange(club) |> 
            dplyr::rename(direction_n = n) |> 
            dplyr::mutate(direction_pct = round((direction_n / sum(direction_n, na.rm = T))*100, 1)), by = c("club")
          
        )
    }
    
    summarize_lie <- function(df) {
      
      df |> 
        dplyr::filter(!is.na(lie)) |> 
        dplyr::mutate(on_target = dplyr::case_when(on_target == 'yes' ~ 1, on_target == 'no' ~ 0, TRUE ~ NA_real_),
                      distance_loss = yds_to_target - yds_traveled) |> 
        dplyr::group_by(lie) |> 
        dplyr::summarize(strokes = dplyr::n(),
                         median_yds_traveled = round(stats::median(yds_traveled, na.rm = T),1),
                         accuracy_pct = round(mean(on_target, na.rm = T)*100, 1),
                         median_yds_lost = round(stats::median(distance_loss, na.rm = T), 1)) |> 
        dplyr::left_join(
          
          df |> 
            dplyr::filter(!is.na(lie)) |> 
            dplyr::mutate(on_target = dplyr::case_when(on_target == 'yes' ~ 1, on_target == 'no' ~ 0, TRUE ~ NA_real_),
                          distance_loss = yds_to_target - yds_traveled) |> 
            dplyr::count(lie, miss_direction) |> 
            dplyr::group_by(lie) |> 
            dplyr::rename(direction_n = n) |> 
            dplyr::mutate(direction_pct = round((direction_n / sum(direction_n, na.rm = T))*100, 1)), by = "lie"
        )
    }
    
    summarize_shottype <- function(df) {
      
      df |> 
        dplyr::filter(!is.na(shot_type)) |> 
        dplyr::filter(!grepl(shot_type, pattern = 'putt|rough')) |> 
        dplyr::mutate(on_target = dplyr::case_when(on_target == 'yes' ~ 1, on_target == 'no' ~ 0, TRUE ~ NA_real_),
                      distance_loss = yds_to_target - yds_traveled) |> 
        dplyr::group_by(shot_type) |> 
        dplyr::summarize(strokes = dplyr::n(),
                         median_yds_traveled = round(stats::median(yds_traveled, na.rm = T),1),
                         accuracy_pct = round(mean(on_target, na.rm = T)*100, 1),
                         median_yds_lost = round(stats::median(distance_loss, na.rm = T), 1))
      
    }
    
    # Populate round dropdown from unique dates ----
    shiny::observe({
      shiny::req(data_r())
      shiny::updateSelectInput(
        session = session,
        inputId = "date",
        choices = data_r()$date |> unique() |> sort(),
        selected = data_r()$date |> max()
      )
    })
    
    # Reactive: Filtered data for selected round ----
    round_data <- shiny::reactive({
      shiny::req(input$date)
      
      data_r() |>
        dplyr::select(-dplyr::contains('js')) |>
        dplyr::filter(date == input$date)
    })
    
    # Populate hole dropdown based on selected round ----
    shiny::observeEvent(round_data(), {
      shiny::updateSelectInput(
        session = session,
        inputId = "hole",
        choices = round_data()$hole |> unique() |> sort(),
        selected = round_data()$hole |> min()
      )
    })
    
    # Reactive: Filtered by hole ----
    hole_data <- shiny::reactive({
      shiny::req(input$hole)
      round_data() |> dplyr::filter(hole == input$hole)
    })
    
    # Outputs ----
    output$round_summary      <- shiny::renderTable(summarize_round(round_data()))
    output$round_metrics      <- shiny::renderTable(summarize_metrics(round_data()))
    output$hole_summary       <- shiny::renderTable(summarize_hole(hole_data()))
    output$hole_metrics       <- shiny::renderTable(summarize_hole_metrics(hole_data()))
    output$club_metrics       <- shiny::renderTable(summarize_club(round_data()))
    output$lie_metrics        <- shiny::renderTable(summarize_lie(round_data()))
    output$shottype_metrics   <- shiny::renderTable(summarize_shottype(round_data()))
    output$shot_table         <- shiny::renderTable(round_data())
    
    # Download handler ----
    output$download_scorecard <- shiny::downloadHandler(
      filename = function() {
        paste0("scorecard_", input$round_id, ".csv")
      },
      content = function(file) {
        readr::write_csv(round_data(), file)
      }
    )
  })
}
