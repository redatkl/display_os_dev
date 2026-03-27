# Page 2 Module: Data

data_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    tags$head(
      tags$style(rel = "stylesheet", type = "text/css", href = "css/data_module.css")
    ),
    
    # div of the config panel
    div(
      class = "config-panel-wrapper",
      
      # ── Left sidebar ──────────────────────────────
      div(
        class = "config-sidebar",
        
        div(class = "title-data", tags$p("Data indices")),
      
        div(class = "title-descr", tags$p("Sélectionnez un indice pour consulter la documentation."))
      ),
      
      # ── Right panel (doc viewer placeholder) ──────
      div(
        class = "doc-panel",
        style = "flex:1; padding: 40px;"
        
      )
    )
  )
}

data_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    

  })
}