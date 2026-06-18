# KPI card helper (CSS-based) ----
kpi_card <- function(label, value, class = NULL) {
  shiny::div(
    class = paste("kpi-card", class),
    shiny::h4(label),
    shiny::h2(value)
  )
}