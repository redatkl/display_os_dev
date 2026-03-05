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
        
        # Icon - Analyse temporelle
        div(
          class      = "sidebar-reporting-icon",
          `data-module` = "analyse_temporelle",
          tags$img(src = "icons_svg/chart-area-solid-full.svg", width = "22px", height = "22px", style = "filter: brightness(0) invert(1);"),
          span(class = "icon-tooltip", "Analyse temporelle")
        ),
        
        # Icon 1 – Classification
        div(
          class      = "sidebar-reporting-icon",
          `data-module` = "classification",
          tags$img(src = "icons_svg/database-solid-full.svg", width = "22px", height = "22px", style = "filter: brightness(0) invert(1);"),
          #icon("database", class = "fa-solid fa-database"),
          span(class = "icon-tooltip", "Classification")
        ),
        
        # Icon 2 – Tableau de bord
        div(
          class      = "sidebar-reporting-icon",
          `data-module` = "dashboard",
          tags$img(src = "icons_svg/dashboard.png", width = "22px", height = "22px", style = "filter: brightness(0) invert(1);"),
          span(class = "icon-tooltip", "Tableau de bord")
        ),
        
        # Icon 3 – Cartes
        div(
          class      = "sidebar-reporting-icon",
          `data-module` = "maps",
          tags$img(src = "icons_svg/morocco.svg", width = "22px", height = "22px", style = "filter: brightness(0) invert(1);"),
          span(class = "icon-tooltip", "Cartes")
        ),
        
        div(class = "sidebar-reporting-divider"),
        
        # Icon 4 – Bulletins
        div(
          class      = "sidebar-reporting-icon",
          `data-module` = "bulletins",
          tags$img(src = "icons_svg/report.png", width = "22px", height = "22px", style = "filter: brightness(0) invert(1);"),
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