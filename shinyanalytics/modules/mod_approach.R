mod_approach_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tabPanel(
    title = "Approach Analysis",
    
    shiny::sidebarLayout(
      
      shiny::sidebarPanel(
        width = 3,
        
        shiny::selectInput(
          inputId = ns("approach_view"),
          label   = "Approach Plot Type\n(Choose One):",
          choices = c(
            "Overall GIR Curve",
            "GIR Curve by Lie",
            "Par-3 GIR Curve by Tee",
            "Par-3 GIR Curve by Course",
            "Custom GIR Analysis"
          ),
          selected = "Overall GIR Curve",
          multiple = F
        ),
        
        shiny::conditionalPanel(
          condition = sprintf("input['%s'] == 'Custom GIR Analysis'", ns("approach_view")),
          shiny::selectInput(
            inputId = ns("group_var"),
            label   = "Group by (Choose One):",
            choices = c("", "par", "lie", "club", "course_name", "tees"),
            selected = "",
            multiple = F
          ),
          shiny::selectInput(
            inputId = ns("facet_var"),
            label   = "Split by (Choose One):",
            choices = c("", "par", "lie", "club", "course_name", "tees"),
            selected = "",
            multiple = F
          ),
          shiny::selectInput(
            inputId = ns("filter_par"),
            label   = "Filter Par (Choose Any):",
            choices = c(3, 4, 5),
            selected = NULL,
            multiple = T
          ),
          shiny::selectInput(
            inputId = ns("filter_lie"),
            label   = "Filter by Lie Type (Choose Any):",
            choices = c("fairway", "rough", "tee"),
            selected = NULL,
            multiple = T
          ),
          shiny::selectInput(
            inputId = ns("filter_club"),
            label   = "Filter by Club (Choose Any):",
            choices = character(0),
            selected = NULL,
            multiple = T
          ),
          shiny::selectInput(
            inputId = ns("filter_course"),
            label   = "Filter by Course (Choose Any):",
            choices = character(0),
            selected = NULL,
            multiple = T
          ),
          shiny::selectInput(
            inputId = ns("filter_tee"),
            label   = "Filter by Tees (Choose Any):",
            choices = character(0),
            selected = NULL,
            multiple = T
          )
        )
      ),
      
      shiny::mainPanel(
        width = 9,
        class = 'main-panel',
        
        shiny::div(
          style = "height: 42vh; min-height: 240px;",
          plotly::plotlyOutput(
            outputId = ns("approach_plot"),
            height = "auto"
          )
        )
        
      )
    )
  )
}

mod_approach_server <- function(id, data_r) {
  shiny::moduleServer(id, function(input, output, session) {
    
    approach_df <- shiny::reactive({
      stroke_level_df <- data_r()
      
      stroke_level_df |>
        dplyr::mutate(
          approach_shots = dplyr::case_when(
            par == 3 & stroke == 1 ~ 1,
            par > 3 &
              shot_type == "full" &
              yds_to_target > 75 &
              club != "D" &
              !grepl(lie, pattern = "sand|tee") ~ 1,
            TRUE ~ 0
          )
        ) |>
        dplyr::filter(approach_shots == 1) |>
        dplyr::mutate(approach_gir = GIR)
    })
    
    # update dynamic filter choices
    shiny::observe({
      df <- approach_df()
      shiny::updateSelectInput(session, "filter_club",
                               choices = sort(unique(df$club))
      )
      shiny::updateSelectInput(session, "filter_course",
                               choices = sort(unique(df$course_name))
      )
      shiny::updateSelectInput(session, "filter_tee",
                               choices = sort(unique(df$tees))
      )
    })
    
    # filtered data for custom mode
    filtered_custom <- shiny::reactive({
      df <- approach_df()
      
      if (length(input$filter_par) > 0) {
        df <- df |> dplyr::filter(par %in% input$filter_par)
      }
      if (length(input$filter_lie) > 0) {
        df <- df |> dplyr::filter(lie %in% input$filter_lie)
      }
      if (!is.null(input$filter_club) && length(input$filter_club) > 0) {
        vals <- setdiff(input$filter_club, "")
        if (length(vals) > 0) {
          df <- df |> dplyr::filter(club %in% vals)
        }
      }
      if (!is.null(input$filter_course) && length(input$filter_course) > 0) {
        vals <- setdiff(input$filter_course, "")
        if (length(vals) > 0) {
          df <- df |> dplyr::filter(course_name %in% vals)
        }
      }
      if (!is.null(input$filter_tee) && length(input$filter_tee) > 0) {
        vals <- setdiff(input$filter_tee, "")
        if (length(vals) > 0) {
          df <- df |> dplyr::filter(tees %in% vals)
        }
      }
      
      df
    })
    
    output$approach_plot <- plotly::renderPlotly({
      req(approach_df())
      
      view <- input$approach_view
      
      if (view == "Custom GIR Analysis") {
        req(filtered_custom())
        make_approach_plot(
          df        = filtered_custom(),
          view      = view,
          group_var = input$group_var,
          facet_var = input$facet_var
        )
      } else {
        make_approach_plot(
          df        = approach_df(),
          view      = view,
          group_var = NULL,
          facet_var = NULL
        )
      }
    })
  })
}
