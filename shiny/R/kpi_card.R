# KPI card helper (CSS-based) ----
kpi_card <- function(label, value) {
  shiny::div(
    class = "kpi-card",
    shiny::h4(label),
    shiny::h2(value)
  )
}