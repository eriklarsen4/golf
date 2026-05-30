library(shiny)
library(bslib)
library(apexcharter)
library(dplyr)
library(lubridate)

# Load module files
source("modules/mod_overview.R")
source("modules/mod_performance.R")
source("modules/mod_approach.R")
source("modules/mod_club.R")
source("modules/mod_lie.R")
source("modules/mod_rounds.R")

# Load analytics functions (NOW inside shiny/R/)
source("R/load_data.R")
source("R/compute_metrics.R")

# Load raw CSVs
raw <- load_data("inst/extdata/golf_exports")

# Build the full stroke-level dataframe (Power BI logic)
stroke_level_df <- compute_stroke_level_df(
  rounds       = raw$rounds,
  courses      = raw$courses,
  club_metrics = raw$club_metrics
)

# App ui
ui <- shiny::fluidPage(
  theme = bslib::bs_theme(version = 5),
  shiny::tabsetPanel(
    shiny::tabPanel("Overview",   mod_overview_ui("overview")),
    shiny::tabPanel("Performance", mod_performance_ui("performance")),
    shiny::tabPanel("Approach",   mod_approach_ui("approach")),
    shiny::tabPanel("Clubs",      mod_club_ui("club")),
    shiny::tabPanel("Lies",       mod_lie_ui("lie")),
    shiny::tabPanel("Rounds",     mod_rounds_ui("rounds"))
  )
)

# App Server
server <- function(input, output, session) {
  
  # Make the dataset reactive for modules
  data_r <- shiny::reactive(stroke_level_df)
  
  mod_overview_server("overview", data_r)
  mod_performance_server("performance", data_r)
  mod_approach_server("approach", data_r)
  mod_club_server("club", data_r)
  mod_lie_server("lie", data_r)
  mod_rounds_server("rounds", data_r)
}

shiny::shinyApp(ui, server)

