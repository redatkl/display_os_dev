# Analyse temporelle module UI
analyse_temporelle_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "css/analyse_temporelle.css")
      ),
    
    div(
      class = "control-bar",
      
      # Indice
      div(class = "control-group",
          tags$span("Indice", class = "control-label"),
          selectInput(ns("indice"), label = NULL,
                      choices = "SPI",
                      selected = "SPI",
                      width = "60px"
          )
      ),
      
      # Niveau
      div(class = "control-group",
          tags$span("Niveau", class = "control-label"),
          selectInput(ns("niveau"), label = NULL,
                      choices = c("National", "Régional", "Provincial", "Communal"),
                      selected = "National",
                      width = "100px"
          ),
          conditionalPanel(
            condition = "input.niveau == 'Régional'",
            ns = ns,
            selectInput(ns("niveau_detail"), label = NULL,
                        choices = regions$nom_fr,
                        selected = regions$nom_fr[grep("Rabat", regions$nom_fr)],
                        width = "210px"
            )
          ),
          conditionalPanel(
            condition = "input.niveau == 'Provincial'",
            ns = ns,
            selectInput(ns("niveau_detail"), label = NULL,
                        choices = na.omit(provinces$Nom_Provinces),
                        selected = provinces$Nom_Provinces[grep("RABAT", provinces$Nom_Provinces)],
                        width = "210px"
            )
          ),
          conditionalPanel(
            condition = "input.niveau == 'Communal'",
            ns = ns,
            selectInput(ns("niveau_detail"), label = NULL,
                        choices = na.omit(communes$commune),
                        selected = communes$commune[grep("Rabat", communes$commune)],
                        width = "210px"
            )
          )
          
      ),
      
      # Search button
      actionButton(ns("search"), label = NULL,
                   icon = icon("magnifying-glass"),
                   class = "btn search-btn"
      )
    )
    )
}

analyse_temporelle_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    observeEvent(input$search, {
      # Trigger analysis with selected filters
      message("Searching: ",
              "Indice=", input$indice,
              " Niveau=", input$niveau,
              " Temporalité=", input$temporalite,
              " Mois=", input$mois,
              " Année=", input$annee
      )
    })
    
    # Return reactive values for use by parent module
    return(reactive({
      list(
        indice      = input$indice,
        niveau      = input$niveau,
        temporalite = input$temporalite,
        mois        = input$mois,
        annee       = input$annee,
        trigger     = input$search
      )
    }))
  })
}