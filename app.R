# Source global file
source("global.R")


# Define UI
ui <- fluidPage(
  
  # Custom CSS styling
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "css/custom.css"),
    # Add favicon in the head section
      tags$link(rel = "shortcut icon", type = "image/png", href = "favicon/favicon.ico"),
    # JavaScript for navigation button styling
    tags$script(src = "js/navigation.js")
  ),
  
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
    
    #center navigation buttons
    tags$div(
      class = "navbar-buttons",
      tags$button(
        id = "nav_accueil",
        class = "nav-btn active",
        onclick = "Shiny.setInputValue('current_page', 'accueil')",
        "Accueil"
      ),
      tags$button(
        id = "nav_data", 
        class = "nav-btn",
        onclick = "Shiny.setInputValue('current_page', 'data')",
        "Data"
      ),
      tags$button(
        id = "nav_expert",
        class = "nav-btn",
        onclick = "Shiny.setInputValue('current_page', 'expert')",
        "Expert"
      ),
      
      tags$button(
        id = "nav_geo", 
        class = "nav-btn",
        onclick = "Shiny.setInputValue('current_page', 'geo')",
        "Geomonitoring"
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
      condition = "input.current_page == 'expert'",
      expert_ui("expert")
    ),
    conditionalPanel(
      condition = "input.current_page == 'geo'",
      geo_ui("geo")
    )
  ),
  

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
  expert_server("expert")
  geo_server("geo")
}

# Run the application
shinyApp(ui = ui, server = server)