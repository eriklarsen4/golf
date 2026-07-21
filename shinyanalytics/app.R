library(shiny)
library(bslib)
library(apexcharter)
library(dplyr)
library(lubridate)

# data prep ----
# overwrite apexcharter's broken renderApexchart
renderApexchart <- function(expr,
                            env = parent.frame(),
                            quoted = FALSE,
                            width = "100%",
                            height = NULL) {
  
  if (!quoted) {
    expr <- substitute(expr)
  }
  
  htmlwidgets::shinyRenderWidget(
    expr,
    apexchartOutput,
    env,
    quoted = TRUE
  )
}


# Load analytics functions
source("R/load_data.R") # loads 'courses', 'rounds', and 'club_metrics'
source("R/compute_metrics.R") # computes round-level aggregate metrics
source("R/kpi_card.R") # for generating the ui parameters of metric values on kpi banners
source("R/make_ts_plot.R") # for making the time series plots for the overview tab
source("R/make_metric_plot.R") # for making the performance (metric) plots for the performance tab
source("R/make_approach_plot.R") # for making complex approach plots
source("R/plot_theme.R")

# Load module files
source("modules/mod_overview.R")
source("modules/mod_performance.R")
source("modules/mod_approach.R")
source("modules/mod_club.R")
source("modules/mod_lie.R")
source("modules/mod_rounds.R")
source("modules/mod_glossary_ui.R")



# Load raw CSVs
raw <- load_data("inst/extdata/golf_exports")

metrics <- compute_all_metrics(raw)

stroke_level_df <- metrics$stroke_level_df
stroke_quality <- metrics$stroke_quality
full_stroke_quality_avg <- metrics$full_stroke_quality_avg
skill_df <- raw$skill_df

# App ui ----
ui <- shiny::navbarPage(
  title = "Golf Analytics",
  
  # import css aesthetics
  shiny::tags$head(
    shiny::tags$link(rel = 'stylesheet', type = 'text/css', href = 'custom.css'),
    shiny::tags$script(src = "https://maxcdn.bootstrapcdn.com/bootstrap/3.4.1/js/bootstrap.min.js")
  ),
  
  # popover activation (info for tabs on mobile)
  shiny::tags$script(shiny::HTML("
  $(function () {
    $('span.tab-info > i.fa-info-circle[data-toggle=\"popover\"]').popover({
      html: true
    });
  });
")),
  
  
  # shiny::tabPanel(
  #   title = "Overview",
  #   icon = shiny::icon('gauge'),
  #   mod_overview_ui("overview")
  # ),
  shiny::tabPanel(
    title = shiny::tags$span(
      "Overview",
      shiny::tags$span(
        class = "tab-info",
        shiny::tags$i(
          class = "fa fa-info-circle",
          tabindex = "0",
          `data-toggle`   = "popover",
          `data-trigger`  = "focus",
          `data-placement`= "bottom",
          `data-content`  = "High-level summary of recent performance across the entire dataset with quick insights into scoring and trends"
        )
      )
    ),
    icon = shiny::icon('gauge'),
    mod_overview_ui("overview")
  ),
  
  
  # shiny::tabPanel(
  #   title = 'Round Metrics',
  #   icon = shiny::icon('chart-bar'),
  #   mod_performance_ui("performance")
  # ),
  shiny::tabPanel(
    title = shiny::tags$span(
      "Round Metrics",
      shiny::tags$span(
        class = "tab-info",
        shiny::tags$i(
          class = "fa fa-info-circle",
          tabindex = "0",
          `data-toggle`   = "popover",
          `data-trigger`  = "focus",
          `data-placement`= "bottom",
          `data-content`  = "Track how round-level statistics—including FIR, GIR, putting, penalties—and scoring change over time"
        )
      )
    ),
    icon = shiny::icon("chart-bar"),
    mod_performance_ui("performance")
  ),
  
  
  # shiny::tabPanel(
  #   title = "Approach Analysis",
  #   icon = shiny::icon('crosshairs'),
  #   mod_approach_ui("approach")
  # ),
  shiny::tabPanel(
    title = shiny::tags$span(
      "Approach Analysis",
      shiny::tags$span(
        class = "tab-info",
        shiny::tags$i(
          class = "fa fa-info-circle",
          tabindex = "0",
          `data-toggle`   = "popover",
          `data-trigger`  = "focus",
          `data-placement`= "bottom",
          `data-content`  = "Break down approach-shot performance by distance and target to see accuracy, miss patterns, and tendencies into greens"
        )
      )
    ),
    icon = shiny::icon('crosshairs'),
    mod_approach_ui("approach")
  ),
  
  
  # shiny::tabPanel(
  #   title = "Club Analysis",
  #   mod_club_ui("club")
  # ),
  shiny::tabPanel(
    title = shiny::tags$span(
      "Club Analysis",
      shiny::tags$span(
        class = "tab-info",
        shiny::tags$i(
          class = "fa fa-info-circle",
          tabindex = "0",
          `data-toggle`   = "popover",
          `data-trigger`  = "focus",
          `data-placement`= "bottom",
          `data-content`  = "Analyze performance for each club to identify strengths and weaknesses in dispersion, directional bias, and consistency"
        )
      )
    ),
    mod_club_ui("club")
  ),
  
  
  # shiny::tabPanel(
  #   title = "Lie Analysis",
  #   icon = shiny::icon('golf-ball-tee'),
  #   mod_lie_ui("lie")
  # ),
  shiny::tabPanel(
    title = shiny::tags$span(
      "Lie Analysis",
      shiny::tags$span(
        class = "tab-info",
        shiny::tags$i(
          class = "fa fa-info-circle",
          tabindex = "0",
          `data-toggle`   = "popover",
          `data-trigger`  = "focus",
          `data-placement`= "bottom",
          `data-content`  = "Understand how different lies affect shots by comparing performance from fairway, rough, sand, and recovery situations"
        )
      )
    ),
    icon = shiny::icon('golf-ball-tee'),
    mod_lie_ui("lie")
  ),
  
  
  # shiny::tabPanel(
  #   title = "Data Search",
  #   icon = shiny::icon('magnifying-glass'),
  #   mod_rounds_ui("rounds")
  # ),
  shiny::tabPanel(
    title = shiny::tags$span(
      "Data Search",
      shiny::tags$span(
        class = "tab-info",
        shiny::tags$i(
          class = "fa fa-info-circle",
          tabindex = "0",
          `data-toggle`   = "popover",
          `data-trigger`  = "focus",
          `data-placement`= "bottom",
          `data-content`  = "Search the entire database, filtering by club, lie, distance, course, or date to find specific patterns"
        )
      )
    ),
    icon = shiny::icon('magnifying-glass'),
    mod_rounds_ui("rounds")
  ),
  
  
  # shiny::tabPanel(
  #   title = 'Glossary and Information',
  #   icon = shiny::icon("book-open"),
  #   glossary_ui("glossary")
  # )
  shiny::tabPanel(
    title = shiny::tags$span(
      "Glossary and Information",
      shiny::tags$span(
        class = "tab-info",
        shiny::tags$i(
          class = "fa fa-info-circle",
          tabindex = "0",
          `data-toggle`   = "popover",
          `data-trigger`  = "focus",
          `data-placement`= "bottom",
          `data-content`  = "Definitions of metrics and analytics used in the app, providing clarity on what each statistic means and how it's calculated"
        )
      )
    ),
    icon = shiny::icon('book-open'),
    glossary_ui("glossary")
  )
)


# App Server ----
server <- function(input, output, session) {
  
  # Make the dataset reactive for modules
  data_r <- shiny::reactive({
    stroke_level_df
  })
  
  # stroke quality is by club
  data_stroke <- shiny::reactive({
    stroke_quality 
  })
  
  # club accuracy is by club for full strokes (subset) and in aggregate (per round)
  data_full <- shiny::reactive({
    full_stroke_quality_avg
  })
  
  # round-level Kalman-filtered skill estimates (relative to avg handicap index)
  data_skill <- shiny::reactive({
    skill_df
  })
  
  # Module wiring
  mod_overview_server("overview", data_r = data_r, data_skill = data_skill)
  mod_performance_server("performance", data_r = data_r)
  mod_approach_server("approach", data_r = data_r)
  mod_club_server("club", stroke_quality = data_stroke, full_stroke_quality_avg = data_full)
  mod_lie_server("lie", stroke_level_df = data_r, stroke_quality = data_stroke)
  mod_rounds_server("rounds", data_r = data_r)
}

# ----
shiny::shinyApp(ui, server)

