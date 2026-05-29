library(shiny)
library(bslib)
library(apexcharter)

# Load module files
source("modules/mod_overview.R")
source("modules/mod_performance.R")
source("modules/mod_approach.R")
source("modules/mod_club.R")
source("modules/mod_lie.R")
source("modules/mod_rounds.R")

# Load analytics functions
# (Assumes your analytics code lives in ../../R or similar)
source("../R/load_data.R")
source("../R/compute_metrics.R")
source("../R/compute_skill_curve.R")
source("../R/compute_gir_curves.R")
source("../R/compute_club_stats.R")

# ---- THEME ----
golf_theme <- bslib::bs_theme(
  version = 5,
  bootswatch = "flatly",   # clean white base
  primary = "#003f87",     # deep navy/cobalt
  secondary = "#0057b8",   # brighter cobalt accent
  navbar_bg = "#003f87",   # cobalt header
  navbar_fg = "white",
  navbar_light_color = "white",
  navbar_light_active_color = "#cce0ff",
  base_font = font_google("Inter"),
  heading_font = font_google("Inter")
)

# ---- UI ----
ui <- shiny::navbarPage(
  title = "Golf Analytics",
  theme = golf_theme,
  collapsible = TRUE,
  fluid = TRUE,
  
  shiny::tabPanel("Overview",          mod_overview_ui("overview")),
  shiny::tabPanel("Performance",       mod_performance_ui("performance")),
  shiny::tabPanel("Approach Analysis", mod_approach_ui("approach")),
  shiny::tabPanel("Club Diagnostics",  mod_club_ui("club")),
  shiny::tabPanel("Lie Performance",   mod_lie_ui("lie")),
  shiny::tabPanel("Round Explorer",    mod_rounds_ui("rounds"))
)

# ---- SERVER ----
server <- function(input, output, session) {
  
  # Load data once at startup
  data <- load_data()
  
  # Pass shared data to modules
  mod_overview_server("overview", data)
  mod_performance_server("performance", data)
  mod_approach_server("approach", data)
  mod_club_server("club", data)
  mod_lie_server("lie", data)
  mod_rounds_server("rounds", data)
}

shiny::shinyApp(ui, server)
