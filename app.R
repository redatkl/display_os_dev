# Source global file
source("global.R")


# Define UI
ui <- tagList(
  
  # Custom CSS styling
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "css/custom.css"),
    tags$link(rel = "stylesheet", type = "text/css", href = "css/navigation.css"),
    
    # Add favicon in the head section
    tags$link(rel = "shortcut icon", type = "image/png", href = "favicon/favicon.ico"),
    # JavaScript for navigation button styling
    tags$script(src = "js/navigation.js")
  ),
fluidPage(
  
  # Custom navigation bar using HTML tags
  tags$div(
    class = "navbar",
    
    # add first logo 
    tags$div(
      class= "logo-left",
      tags$a(href = "https://poledigital.ma", target = "_blank", tags$img(src = "logos/logo.png", height = "50px"))
    ),
    # add second logo on the left
    tags$div(
      class= "logo-left",
      tags$a(href = "https://poledigital.ma", target = "_blank", tags$img(src = "logos/logo-pd.png", height = "50px"))
    ),
    
    # Hamburger button (hidden until overflow detected by JS)
    tags$button(
      id = "hamburger-btn",
      class = "hamburger-btn",
      `aria-label` = "Plus de pages",
      tags$span(class = "hamburger-bar"),
      tags$span(class = "hamburger-bar"),
      tags$span(class = "hamburger-bar")
    ),
    # Overflow dropdown (populated by JS)
    tags$div(id = "nav-overflow-dropdown", class = "nav-overflow-dropdown"),
    
    # Overlay
    tags$div(id = "nav-overlay", class = "nav-overlay"),
    
    #center navigation buttons
    tags$div(
      class = "navbar-buttons",
      tags$button(
        id = "nav_accueil",
        class = "nav-btn active",
        `data-page` = "accueil",
        onclick = "Shiny.setInputValue('current_page', 'accueil')",
        "Accueil"
      ),
      tags$button(
        id = "nav_data", 
        class = "nav-btn",
        `data-page` = "data",
        onclick = "Shiny.setInputValue('current_page', 'data')",
        "Data"
      ),
      tags$button(
        id = "nav_station",
        class = "nav-btn",
        `data-page` = "station",
        onclick = "Shiny.setInputValue('current_page', 'station')",
        "Stations"
      ),
      
      tags$button(
        id = "nav_geo", 
        class = "nav-btn",
        `data-page` = "geo",
        onclick = "Shiny.setInputValue('current_page', 'geo')",
        "Geomonitoring"
      ),
      
      tags$button(
        id = "nav_forecast", 
        class = "nav-btn",
        `data-page` = "forecast",
        onclick = "Shiny.setInputValue('current_page', 'forecast')",
        "Prévisions"
      ),
      
      tags$button(
        id = "nav_reporting", 
        class = "nav-btn",
        `data-page` = "reporting",
        onclick = "Shiny.setInputValue('current_page', 'reporting')",
        "Reporting"
      )
    ),
    # first logo on the right 
    tags$div(
      class= "logo-right",
      tags$a(href = "https://www.agriculture.gov.ma/", target = "_blank", tags$img(src = "logos/logo_map.png", height = "50px"))
    ),
    # second logo on the right
    tags$div(
      class= "logo-right",
      tags$a(href = "https://www.agriculture.gov.ma/fr/ministere/generation-green-2020-2030", target = "_blank", tags$img(src = "logos/logo_gen_green.png", height = "50px"))
    )
  ),
  
  # Page content container
  tags$div(
    class = "page-content",
    conditionalPanel(
      condition = "input.current_page == 'accueil' || typeof input.current_page === 'undefined'",
      accueil_ui("accueil")
    ),
    conditionalPanel(
      condition = "input.current_page == 'data'",
      data_ui("data")
    ),
    conditionalPanel(
      condition = "input.current_page == 'station'",
      station_ui("station")
    ),
    conditionalPanel(
      condition = "input.current_page == 'geo'",
      geo_ui("geo")
    ),
    conditionalPanel(
      condition = "input.current_page == 'forecast'",
      forecast_ui("forecast")
    ),
    conditionalPanel(
      condition = "input.current_page == 'reporting'",
      reporting_ui("reporting")
    )
  )
),
# Footer
tags$footer(
  class = "footer",
  tags$div(
    class = "footer-content",
    # Left section - Copyright
    tags$div(
      class = "footer-left",
      tags$p("")
    ),
    # Center section - Logo and text
    tags$div(
      class = "footer-center",
      tags$img(src = "logos/logo_footer.png", height = "20px", class = "footer-logo"),
      tags$p(class = "footer-text", "Pôle Digital de l'Agriculture, de la Forêt et Observatoire de la Sécheresse")
    ),
    # Right section - Can be empty or add content
    tags$div(
      class = "footer-right",
      tags$p(paste0("Tous droits réservés ©", year_footer))
    )
  )
)
)


# Define server
server <- function(input, output, session) {
  observe(
    # Initialize current page
    if (is.null(input$current_page)) {
      updateSelectInput(session, "current_page", selected = "accueil")
    })
  
  # Call module servers
  accueil_server("accueil")
  data_server("data")
  station_server("station")
  geo_server("geo")
  forecast_server("forecast")
  reporting_server("reporting")
}

# Run the application
shinyApp(ui = ui, server = server)