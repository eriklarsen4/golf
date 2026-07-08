mod_rounds_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tabPanel(
    title = "Rounds",
    
    shiny::sidebarLayout(
      
      shiny::sidebarPanel(
        width = 3,
        
        shiny::selectInput(
          inputId = ns('tableChoice'),
          label = "Select Table(s) to View: (Choose Any)",
          choices = c(
            "Round Summary" = "round_summary",
            "Round Metrics" = "round_metrics",
            "Hole Summary" = "hole_summary",
            "Hole Metrics" = "hole_metrics",
            "Club Metrics" = "club_metrics",
            "Lie Metrics" = "lie_metrics",
            "Shot-Type Metrics" = "shottype_metrics",
            "Shot List" = "shot_table"
          ),
          multiple = T,
          selected = c("round_summary", "round_metrics")
        ),
        
        shiny::selectInput(
          inputId = ns("date"),
          label   = "Filter by Date (Choose 'All' OR Any)",
          choices = NULL,
          selected = NULL,
          multiple = T
        ),
        
        shiny::selectInput(
          inputId = ns("course"),
          label = "Filter by Course (Choose 'All' OR Any)",
          choices = c("All", "Randolph North", "Dell Urich", "Silverbell", "El Rio", "Fred Enke", "Sewailo", "Arizona National", "Crooked Tree"),
          selected = NULL,
          multiple = T
        ),
        
        shiny::selectInput(
          inputId = ns("hole"),
          label   = "Filter by Holes (Choose 'All' OR Any)",
          choices = c('All', c(seq(1,18,1))),
          selected = 'All',
          multiple = T
        ),
        
        shiny::selectInput(
          inputId = ns("club_filter"),
          label   = "Filter by Club (Choose 'All' OR Any)",
          choices = c("All", "P", "LW", "SW", "GW", "PW", "9", "8", "7", "6", "5", "4", "3W", "D"),
          selected = "All",
          multiple = T
        ),
        
        shiny::selectInput(
          inputId = ns("shot_filter"),
          label = "Filter by Shot-Type (Choose 'All' OR Any)",
          choices = c("All", 'tee', 'full', 'gsbunker', 'fwbunker', 'chip', 'punch', 'choked'),
          selected = "All",
          multiple = T
        ),
        
        shiny::selectInput(
          inputId = ns("lie_filter"),
          label   = "Filter by Lie Type (Choose 'All' OR Any)",
          choices = c("All", "tee", "fairway", "rough", "sand"),
          selected = "All",
          multiple = T
        ),
        
        shiny::downloadButton(
          outputId = ns("download_scorecard"),
          label    = "Download Scorecard"
        )
        
      ),
      shiny::mainPanel(
        width = 9,
        
        div(
          class = 'table_scroll',
          style = 'overflow-x: auto; width: 100%',
          shiny::uiOutput(ns("tables_container"))
        )
      )
    )
  )
}

mod_rounds_server <- function(id, data_r) {
  shiny::moduleServer(id, function(input, output, session) {
    
    summarize_round <- function(df) {
      df |> 
        dplyr::select(c(player, handicap_index, course, date, tees), dplyr::contains("tot_"), fir, gir, updown) |> 
        dplyr::distinct()
    }
    
    summarize_metrics <- function(df) {
      
      # Ensure required columns exist
      required_cols <- c(
        "putts", "GIR", "stroke", "yds_traveled", "yds_to_target",
        "shot_type", "par", "club", "lie"
      )
      
      for (col in required_cols) {
        if (!col %in% names(df)) df[[col]] <- NA
      }
      
      df |>
        dplyr::mutate(
          GIR_putts = dplyr::case_when(GIR == 1 ~ putts, TRUE ~ NA_real_),
          approach_shots = dplyr::case_when(
            par == 3 & stroke == 1 ~ 1,
            par > 3 &
              shot_type == "full" &
              yds_to_target > 75 &
              club != "D" &
              !grepl(lie, pattern = "sand|tee") ~ 1,
            TRUE ~ 0
          ),
          tee_shot_distance = dplyr::case_when(
            stroke == 1 & par > 3 ~ yds_traveled,
            TRUE ~ NA_real_
          ),
          approach_distance = dplyr::case_when(
            approach_shots == 1 ~ yds_traveled,
            TRUE ~ NA_real_
          )
        ) |>
        dplyr::group_by(player, handicap_index, date, course, tees) |>
        dplyr::summarize(
          avg_tee_shot_distance = mean(tee_shot_distance, na.rm = TRUE),
          avg_approach_distance = mean(approach_distance, na.rm = TRUE),
          avg_GIR_putts         = mean(GIR_putts, na.rm = TRUE),
          avg_putts             = mean(putts, na.rm = TRUE),
          .groups = "drop"
        ) |>
        dplyr::mutate(
          dplyr::across(
            dplyr::where(is.numeric),
            ~ ifelse(is.nan(.x), NA_real_, .x)
          )
        )
      
    }
    
    summarize_hole <- function(df){
      df |> 
        dplyr::select(c(player_name, handicap_index, date, course, tees, hole), par, gross, net, FIR, GIR, putts, penalties) |> 
        dplyr::distinct() |> 
        dplyr::group_by(course, hole) |> 
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
        dplyr::select(c(player_name, handicap_index, date, course, tees, hole), stroke, GIR, FIR, chips, putts, scrambles, on_target, dplyr::starts_with("is_")) |> 
        dplyr::group_by(course, hole, tees, stroke) |> 
        dplyr::mutate(non_putts_on_target = sum(on_target, na.rm = T)) |> 
        dplyr::select(-stroke, -on_target) |> 
        dplyr::distinct() |> 
        dplyr::group_by(course, tees, hole) |> 
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
        dplyr::distinct() |> 
        dplyr::ungroup()
    }
    
    summarize_club <- function(df) {
      
      club_levels <- c("LW","SW","GW","PW","9","8","7","6","5","4","3W","D")
      
      # Ensure required columns exist
      safe_cols <- c("stroke","club","shot_type","yds_traveled","yds_to_target",
                     "miss_direction","on_target","player_name","handicap_index",
                     "date","course","tees","hole")
      
      for (col in safe_cols) {
        if (!col %in% names(df)) df[[col]] <- NA
      }
      
      df <- df |>
        dplyr::mutate(
          distance_loss = yds_to_target - yds_traveled,
          on_target = dplyr::case_when(
            on_target == "yes" ~ 1,
            on_target == "no"  ~ 0,
            TRUE ~ NA_real_
          ),
          club = factor(club, levels = club_levels)
        )
      
      # Filter only if club data exists
      df_filtered <- df |>
        dplyr::filter(
          !is.na(club),
          club != "P",
          shot_type %in% c("full","tee","chip")
        )
      
      # If no club strokes exist, return NA summary row
      if (nrow(df_filtered) == 0) {
        return(
          df |>
            dplyr::summarize(
              strokes = NA_integer_,
              median_yds_traveled = NA_real_,
              accuracy_pct = NA_real_,
              median_yds_lost = NA_real_
            )
        )
      }
      
      # Main summary
      main_summary <- df_filtered |>
        dplyr::group_by(player_name, club) |>
        dplyr::summarize(
          strokes = dplyr::n(),
          median_yds_traveled = round(stats::median(yds_traveled, na.rm = TRUE), 1),
          accuracy_pct = round(mean(on_target, na.rm = TRUE) * 100, 1),
          median_yds_lost = round(stats::median(distance_loss, na.rm = TRUE), 1),
          .groups = "drop"
        )
      
      # Miss direction summary
      miss_summary <- df_filtered |>
        dplyr::group_by(club, miss_direction) |>
        dplyr::summarize(direction_n = dplyr::n(), .groups = "drop") |>
        dplyr::group_by(club) |>
        dplyr::mutate(direction_pct = round(direction_n / sum(direction_n) * 100, 1))
      
      dplyr::left_join(main_summary, miss_summary, by = "club")
      # club_levels <- c("LW", "SW", "GW", "PW", "9", "8", "7", "6", "5", "4", "3W", "D")
      # 
      # df |>
      #   dplyr::filter(!is.na(stroke), 
      #                 !is.na(club), 
      #                 club != 'P',
      #                 shot_type %in% c('full', 'tee', 'chip')) |> 
      #   dplyr::mutate(distance_loss = yds_to_target - yds_traveled,
      #                 on_target = dplyr::case_when(on_target == 'yes' ~ 1, on_target == 'no' ~ 0, TRUE ~ NA_real_),
      #                 club = factor(club, levels = club_levels)
      #   ) |> 
      #   dplyr::select(c(player_name, handicap_index, date, course, tees, hole, stroke, club, yds_traveled, distance_loss, miss_direction, on_target)) |> 
      #   dplyr::group_by(player_name, club) |> 
      #   dplyr::summarize(strokes = dplyr::n(),
      #                    median_yds_traveled = round(stats::median(yds_traveled, na.rm = T),1),
      #                    accuracy_pct = round(mean(on_target, na.rm = T)*100, 1),
      #                    median_yds_lost = round(stats::median(distance_loss, na.rm = T), 1)) |> 
      #   dplyr::left_join(
      #     
      #     df |> 
      #       dplyr::filter(!is.na(stroke), 
      #                     !is.na(club), 
      #                     club != 'P',
      #                     shot_type %in% c('full', 'tee', 'chip')) |> 
      #       dplyr::mutate(distance_loss = yds_to_target - yds_traveled,
      #                     on_target = dplyr::case_when(on_target == 'yes' ~ 1, on_target == 'no' ~ 0, TRUE ~ NA_real_),
      #                     club = factor(club, levels = club_levels)
      #       ) |> 
      #       dplyr::select(c(player_name, handicap_index, date, course, tees, hole, stroke, club, yds_traveled, distance_loss, miss_direction, on_target)) |> 
      #       dplyr::group_by(club) |> 
      #       dplyr::count(club, miss_direction) |> 
      #       dplyr::group_by(club) |> 
      #       dplyr::rename(direction_n = n) |> 
      #       dplyr::mutate(direction_pct = round((direction_n / sum(direction_n, na.rm = T))*100, 1)), by = c("club")
      #     
      #   )
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
            dplyr::mutate(direction_pct = round((direction_n / sum(direction_n, na.rm = T))*100, 1)), by = 'lie'
        ) |> 
        dplyr::arrange(desc(strokes))
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
        session,
        "date",
        choices = c("All", data_r()$date |> base::format("%Y-%m-%d") |> base::unique() |> base::sort()),
        selected = "All"
      )
    })
    
    # Populate round dropdown from unique courses ----
    shiny::observe({
      shiny::req(data_r())
      shiny::updateSelectInput(
        session,
        "course",
        choices = c("All", base::unique(data_r()$course_name)),
        selected = "All"
      )
    })
    
    # Reactive: Filtered data for selected round/date or course----
    round_data <- shiny::reactive({
      shiny::req(data_r())
      df <- data_r()
      
      if (!("All" %in% input$date)) {
        df <- df |> dplyr::filter(date %in% input$date)
      }
      
      if (!("All" %in% input$course)) {
        df <- df |> dplyr::filter(course_name %in% input$course)
      }
      
      if (!("All" %in% input$club_filter) &&
          base::length(input$club_filter) > 0 &&
          "club" %in% base::names(df) &&
          base::any(!base::is.na(df$club))) {
        
        df <- df |> dplyr::filter(club %in% input$club_filter)
      }
      
      if (!("All" %in% input$lie_filter)) {
        df <- df |> dplyr::filter(base::grepl(lie, pattern = "tee|fairway|rough|sand"))
        df <- df |> dplyr::filter(lie %in% input$lie_filter)
      }
      
      df |>
        dplyr::mutate(
          date = base::format(date, "%Y-%m-%d"),
          date = base::as.character(date)
        ) |>
        dplyr::select(-player_id) |>
        dplyr::rename(course = course_name)
    })
    
    # Populate hole dropdown based on selected round ----
    shiny::observeEvent(round_data(), {
      shiny::updateSelectInput(
        session,
        "hole",
        choices = c("All", round_data()$hole |> base::unique() |> base::sort()),
        selected = shiny::isolate(input$hole)
      )
    })
    
    # Reactive: Filtered by hole ----
    hole_data <- shiny::reactive({
      shiny::req(round_data())
      df <- round_data()
      
      if (!("All" %in% input$hole) && base::length(input$hole) > 0) {
        df <- df |> dplyr::filter(hole %in% input$hole)
      }
      
      if (!("All" %in% input$shot_filter) &&
          base::length(input$shot_filter) > 0 &&
          "shot_type" %in% base::names(df) &&
          base::any(!base::is.na(df$shot_type))) {
        
        df <- df |> dplyr::filter(shot_type %in% input$shot_filter)
      }
      
      df
    })
    
    # Populate shot-type dropdown based on selected shot type and hole ----
    shiny::observeEvent(hole_data(), {
      df <- hole_data()
      
      valid_shots <- df |>
        dplyr::filter(
          !base::is.na(shot_type),
          base::grepl(shot_type, pattern = "tee|full|punch|choked|chip|bunker")
        ) |>
        dplyr::pull(shot_type) |>
        base::unique()
      
      if (base::length(valid_shots) == 0) {
        valid_shots <- "All"
      }
      
      shiny::updateSelectInput(
        session,
        "shot_filter",
        choices = c("All", valid_shots),
        selected = shiny::isolate(input$shot_filter)
      )
    })
    
    # Outputs ----
    output$tables_container <- shiny::renderUI({
      req(input$tableChoice)
      
      shiny::tagList(
        lapply(input$tableChoice, function(tbl){
          shiny::tableOutput(session$ns(tbl))
        })
      )
    })
    
    output$round_summary <- shiny::renderTable({
      df <- round_data() |> dplyr::rename(player = player_name)
      req(nrow(df)> 0) 
      summarize_round(df)
      })
    output$round_metrics <- shiny::renderTable({
      df <- round_data() |> dplyr::rename(player = player_name)
      req(nrow(df)>0)
      summarize_metrics(df)
    })
    output$hole_summary <- shiny::renderTable({
      df <- hole_data()
      req(nrow(df)>0)
      summarize_hole(df)
      })
    output$hole_metrics <- shiny::renderTable({
      df <- hole_data()
      req(nrow(df)>0)
      summarize_hole_metrics(df)
      })
    output$club_metrics <- shiny::renderTable({
      df <- round_data()
      req(nrow(df)>0)
      summarize_club(df)
      })
    output$lie_metrics <- shiny::renderTable({
      df <- round_data()
      req(nrow(df)>0)
      summarize_lie(df)
      })
    output$shottype_metrics <- shiny::renderTable({
      df <- round_data()
      req(nrow(df)>0)
      summarize_shottype(df)
      })
    output$shot_table <- shiny::renderTable({
      df <- round_data() |> 
        dplyr::select(-GHIN) |> 
        dplyr::rename(player = player_name) |> 
        dplyr::relocate(date, .after = course) |> 
        dplyr::select(-tee_club, -date_js) |> 
        dplyr::group_by(player, course, tees, date, hole) |> 
        dplyr::arrange(date, hole, stroke) |> 
        dplyr::ungroup()
      req(nrow(df)>0)
      df
    })
    
    # Download handler ----
    output$download_scorecard <- shiny::downloadHandler(
      filename = function() {
        paste0("scorecard_", input$date, ".csv")
      },
      content = function(file) {
        readr::write_csv(round_data(), file)
      }
    )
  })
}
