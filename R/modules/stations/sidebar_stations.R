# sidebar module in the page stations

sidebar_stations_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    # Include custom CSS and JS
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "css/sidebar_stations.css"),
      tags$script(src = "js/sidebar_stations.js"),
      tags$script(src = "js/toggle_button.js"),
      tags$script(src = "js/datepicker.js")
    ),
    
    # Sidebar container
    div(
      id = ns("sidebar-container"),
      class = "custom-sidebar stations-sidebar",
      
      # Icon buttons
      div(
        class = "sidebar-icons",
        
        div(
          class = "sidebar-icon active",
          `data-section` = "source",
          icon("database"),
          span(class = "icon-tooltip", "Source de données")
        ),
        
        div(
          class = "sidebar-icon",
          `data-section` = "variable",
          icon("chart-line"),
          span(class = "icon-tooltip", "Variable")
        ),
        
        div(
          class = "sidebar-icon",
          `data-section` = "periode",
          icon("calendar"),
          span(class = "icon-tooltip", "Période")
        )
      ),
        
        
      # Single panel always expanded
      div(
        class = "sidebar-panels expanded",
        div(
          class = "sidebar-panel active",
          
          # Section 1: Source
          div(
            id = ns("section-source"),
            class = "panel-section active",
            `data-section` = "source",
            div(class = "section-header",
                icon("database"), 
                span("Source de données")
            ),
            toggle_switch_group(
              group_id = ns("data_source"),
              options = list("station1" = "Station 1", "station2" = "Station 2"),
              selected = "station1"
            )
          ),
          
          # Section 2: Variable
          div(
            id = ns("section-variable"),
            class = "panel-section",
            `data-section` = "variable",
            div(class = "section-header",
                icon("chart-line"), 
                span("Variable")
            ),
            toggle_switch_group(
              group_id = ns("variable"),
              options = list("temp" = "Température", "precip" = "Précipitations"),
              selected = "temp"
            )
          ),
          
          # Section 3: Période
          div(
            id = ns("section-periode"),
            class = "panel-section",
            `data-section` = "periode",
            div(class = "section-header",
                icon("calendar"), 
                span("Période")
            ),
            customDatePickerInput(ns("date_range"), value = Sys.Date())
          )
        )
      )
    )
  )
}

# sidebar stations server
sidebar_stations_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
   
  })
}