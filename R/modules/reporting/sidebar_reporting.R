# R/modules/reporting/sidebar_reporting.R
# Reporting sidebar — mirrors the structure of sidebar_forecast.R
# and sidebar_stations.R for a consistent UX.

# ── UI ─────────────────────────────────────────────────────────────────────────
sidebar_reporting_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "css/sidebar_reporting.css"),
      tags$script(src = "js/sidebar_reporting.js")
    ),
    
    div(
      id    = ns("sidebar-reporting-container"),
      class = "custom-sidebar reporting-sidebar",
      
      # ── Icon strip ────────────────────────────────────────────────────────────
      div(
        class = "sidebar-reporting-icons",
        
        # Icon 1 – Classification
        div(
          class      = "sidebar-reporting-icon",
          `data-module` = "classification",
          icon("database", class = "fa-solid fa-database"),
          span(class = "icon-tooltip", "Classification")
        ),
        
        # Icon 2 – Tableau de bord
        div(
          class      = "sidebar-reporting-icon",
          `data-module` = "dashboard",
          icon("chart-area", class = "fa-solid fa-chart-area"),
          span(class = "icon-tooltip", "Tableau de bord")
        ),
        
        # Icon 3 – Cartes
        div(
          class      = "sidebar-reporting-icon",
          `data-module` = "maps",
          tags$img(src = "icons_svg/morocco.svg", width = "32px", height = "32px"),
          span(class = "icon-tooltip", "Cartes")
        ),
        
        div(class = "sidebar-reporting-divider"),
        
        # Icon 4 – Bulletins
        div(
          class      = "sidebar-reporting-icon",
          `data-module` = "bulletins",
          tags$img(src = "icons_svg/reports_icon.svg", width = "32px", height = "32px"),
          span(class = "icon-tooltip", "Bulletins")
        )
      )
      ) 
    ) 
}


# ── Server ─────────────────────────────────────────────────────────────────────
sidebar_reporting_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
   
  })
}