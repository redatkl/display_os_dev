# CLassification module UI
classification_ui <- function(id) {
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
                      choices = c(
                        "SPI"     = "SPI",
                        "LST"     = "LST",
                        "ALST"    = "LST_A",
                        "NDVI"    = "NDVI",
                        "ANDVI"   = "ANDVI",
                        "SM"      = "SM",
                        "SM_ROOT" = "SM_ROOT",
                        "ASM"     = "SM_A_SURFACE",
                        "ASM_ROOT"= "SM_A_ROOT",
                        "CDI"     = "CDI"
                      ),
                      selected = "SPI",
                      width = "120px"
          )
      ),
      
      # Niveau
      div(class = "control-group",
          tags$span("Niveau", class = "control-label"),
          selectInput(ns("niveau"), label = NULL,
                      choices = c("National", "Régional", "Provincial", "Communal"),
                      selected = "National",
                      width = "120px"
          ),
          conditionalPanel(
            condition = "input.niveau == 'Régional'",
            ns = ns,
            selectInput(ns("region_detail"), label = NULL,
                        choices = regions$nom_fr,
                        selected = NULL,
                        width = "210px"
            )
          ),
          
          conditionalPanel(
            condition = "input.niveau == 'Provincial'",
            ns = ns,
            div(
              style = "display: flex; align-items: center; gap: 6px;",
              # First: pick a region
              selectInput(ns("region_filter"), label = NULL,
                          choices = na.omit(unique(regions$nom_fr)),
                          selected = na.omit(unique(regions$nom_fr))[1],
                          width = "210px"
              ),
              # Second: provinces filtered by region (updated server-side)
              selectInput(ns("province_detail"), label = NULL,
                          choices = NULL,  
                          width = "210px"
              )
            )),
          
          conditionalPanel(
            condition = "input.niveau == 'Communal'",
            ns = ns,
            div(
              style = "display: flex; align-items: center; gap: 6px;",
              # First: pick a region
              selectInput(ns("region_commune_filter"), label = NULL,
                          choices = na.omit(unique(regions$nom_fr)),
                          selected = na.omit(unique(regions$nom_fr))[1],
                          width = "210px"
              ),
              # Second: provinces filtered by region (updated server-side)
              selectInput(ns("province_commune_filter"), label = NULL,
                          choices = NULL,  
                          width = "210px"
              ),
              selectInput(ns("commune_detail"), label = NULL,
                          choices = NULL,
                          selected = NULL,
                          width = "210px"
              )
            )
          )
          
      ),
      
      # Temporalité 
      div(class = "control-group",
          tags$span("Temporalité", class = "control-label"),
          selectInput(ns("temporalite"), label = NULL,
                      choices = c(
                        "Mensuel"      = "mensuel",
                        "Trimestriel"  = "trimestriel",
                        "Annuel"       = "annuel"
                      ),
                      selected = "mensuel",
                      width    = "120px"
          )
      ),
      
      # Search button
      actionButton(ns("search"), label = NULL,
                   icon = icon("magnifying-glass"),
                   class = "btn search-btn"
      )    
    ),
    
    # Figure display area
    div(
      class = "table-area",
      DT::dataTableOutput(ns("table_display"))
    )
  )
  
}

classification_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    observeEvent(input$search, {
      # Trigger analysis with selected filters
      message("Searching: ",
              "Indice=", input$indice,
              " Niveau=", input$niveau,
              " Région=", input$region_detail,
              " Province=", input$province_detail,
              " Commune=", input$commune_detail)
    })
    
    # When region changes, update provinces list
    update_provinces <- function(region_name) {
      req(region_name, nchar(region_name) > 0)
      code_region <- paste0(sprintf("%02d", regions$id_region[regions$nom_fr == region_name]), ".")
      filtered_provinces <- na.omit(provinces$Nom_Provinces[provinces$Code_Region == code_region])
      updateSelectInput(session, "province_detail", choices  = filtered_provinces, selected = filtered_provinces[1])
    }
    
    # Provinces for Communal panel
    update_communes_provinces <- function(region_name) {
      req(region_name, nchar(region_name) > 0)
      code_region <- paste0(sprintf("%02d", regions$id_region[regions$nom_fr == region_name]), ".")
      filtered_provinces <- na.omit(provinces$Nom_Provinces[provinces$Code_Region == code_region])
      updateSelectInput(session, "province_commune_filter",
                        choices = filtered_provinces, selected = filtered_provinces[1])
    }
    
    # Communes filtered by province
    update_communes <- function(province_name) {
      req(province_name, nchar(province_name) > 0)
      filtered_communes <- na.omit(
        commune_province_map$commune[commune_province_map$Nom_Provinces == province_name]
      )
      updateSelectInput(session, "commune_detail",
                        choices = filtered_communes, selected = filtered_communes[1])
    }
    
    # Provincial observers
    observeEvent(input$niveau, {
      if (input$niveau == "Provincial") {
        update_provinces(input$region_filter)
      }
    })
    observeEvent(input$region_filter, {
      if (input$niveau == "Provincial") {
        update_provinces(input$region_filter)
      }
    })
    
    # Communal observers - level 1: region → province
    observeEvent(input$niveau, {
      if (input$niveau == "Communal") update_communes_provinces(input$region_commune_filter)
    })
    observeEvent(input$region_commune_filter, {
      if (isTruthy(input$niveau) && input$niveau == "Communal") update_communes_provinces(input$region_commune_filter)
    })
    # Communal observers - level 2: province → commune
    observeEvent(input$province_commune_filter, {
      if (isTruthy(input$niveau) && input$niveau == "Communal") update_communes(input$province_commune_filter)
    })
    
    # Render table
    output$table_display <- DT::renderDataTable({
      # Placeholder data - replace with actual query results
      data.frame(
        Indice = c("SPI", "SPI", "SPI"),
        Niveau = c("National", "Régional", "Provincial"),
        Région = c(NA, input$region_detail, input$region_filter),
        Province = c(NA, NA, input$province_detail),
        Commune = c(NA, NA, NA)
      )
    })
    
    # Return reactive values for use by parent module
    return(reactive({
      list(
        indice      = input$indice,
        niveau      = input$niveau,
        trigger     = input$search
      )
    }))
  })
}