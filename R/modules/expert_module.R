# Expert module


expert_ui <- function(id) {
  ns <- NS(id)
  
  tags$h2("Expert Page - Under Construction", class = "page-title")
  
}

expert_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Server logic for Expert module goes here
  })
}