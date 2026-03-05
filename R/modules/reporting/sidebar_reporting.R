# sidebar for the reporting module

# ui part
sidebar_reporting_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # Include custom CSS and JS
    # tags$head(
    # 
    # ),
    
    # Sidebar container
    div(
      id = ns("sidebar-reporting-container"),
      class = "custom-sidebar-reporting",
      
      # Icon buttons
      div(
        class = "sidebar-reporting-icons",
        
        # Icon 1 - classifications
        div(
          class = "sidebar-reporting-icon",
          icon("database", class = "fa-solid fa-database"),
          span(class = "icon-tooltip", "Classification")
        ),
        
        # Icon 2 - tableau de bord
        div(
          class = "sidebar-reporting-icon",
          icon("chart-area", class = "fa-solid fa-chart-area"),
          span(class = "icon-tooltip", "Tableau de bord")
        ),
        
        # Icon 3 - Analytics
        div(
          class = "sidebar-reporting-icon",
          tags$img(src = "icons_svg/morocco.svg", width = "32px", height = "32px"),
          span(class = "icon-tooltip", "cartes")
        ),
        
        # split panel icons with a line
        div(class = "sidebar-reporting-divider"),
        
        # Icon 4 - bulletins
        div(
          class = "sidebar-reporting-icon",
          tags$img(src = "icons_svg/reports_icon.svg", width = "32px", height = "32px"),
          span(class = "icon-tooltip", "bulletins")
        )
        
      )
    )
  )
  
}

# server part
sidebar_reporting_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
  })
}