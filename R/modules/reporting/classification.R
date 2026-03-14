source("R/functions/db_config.R")
source("R/functions/classification_table.R")

# CLassification module UI
classification_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "css/analyse_temporelle.css")
    ),
    
    div(
      class = "control-bar",
      style = "display: flex; align-items: stretch; gap: 12px;",
      
      # Column: Row1 (Indice+Niveau) and Row2 (Temporalité)
      div(style = "display: flex; flex-direction: column; gap: 6px;",
      
      # Indice
      # Row 1: Indice + Niveau  ← both on same row
      div(style = "display: flex; align-items: center; gap: 12px;",
          
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
      )
      ),
      
      # Row 2: Temporalité
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
          ),
          # Mois — hidden when Annuel
          conditionalPanel(
            condition = "input.temporalite == 'mensuel'",
            ns = ns,
            selectInput(ns("mois"), label = NULL,
                        choices = c(
                          "Janvier" = 1, "Février" = 2, "Mars" = 3,
                          "Avril" = 4, "Mai" = 5, "Juin" = 6,
                          "Juillet" = 7, "Août" = 8, "Septembre" = 9,
                          "Octobre" = 10, "Novembre" = 11, "Décembre" = 12
                        ),
                        selected = as.integer(format(Sys.Date(), "%m")),
                        width = "100px"
            )
          ),
          
          # Trimestre — when choice is trimestre
          conditionalPanel(
            condition = "input.temporalite == 'trimestriel'",
            ns = ns,
            selectInput(ns("trimestre"), label = NULL,
                        choices = c(
                          "T1" = 1, "T2" = 2, "T3" = 3,
                          "T4" = 4
                        ),
                        selected = "T1",
                        width = "100px"
            )
          ),
          
          # Année
          selectInput(ns("annee"), label = NULL,
                      choices  = seq(1981, as.integer(format(Sys.Date(), "%Y"))),
                      selected = as.integer(format(Sys.Date(), "%Y")),
                      width    = "80px"
          )
      )
      ),
      
      # Right side: search button centered vertically
      div(style = "display: flex; align-items: center;",
      actionButton(ns("search"), label = NULL,
                   icon = icon("magnifying-glass"),
                   class = "btn search-btn"
      )
      )   

    ),
    
    # Figure display area
    div(
      class = "table-area",
      style = "margin-top: 20px; position: relative;",
      shinycssloaders::withSpinner(
      DT::dataTableOutput(ns("table_display")),
      type    = 4,        # spinner style 1-8
      color   = "#4a7c59", #  green color
      size    = 0.5,
      caption = tags$img(
        src = "logos/logo.png", height = "30px", style = "
        position: absolute;
        top: 50%;
        left: 50%;
        transform: translate(-50%, -50%);
        height: 30px;
        margin-top: 0;
      "
      )
      )
    )
  )
}

classification_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    observeEvent(input$search, {
      # Trigger analysis with selected filters
      message("Searching: ",
              "Indice=",       input$indice,
              " Temporalite=", input$temporalite,
              " Niveau=",      input$niveau,
              " Mois=",        input$mois,
              " Trimestre=",   input$trimestre,
              " Annee=",       input$annee)
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
      req(input$search > 0)
      
      isolate({
        req(input$indice, input$temporalite, input$annee)
        
        date_str <- build_date_str(input$temporalite, input$mois, input$trimestre, input$annee)
        
        conn <- init_db()
        on.exit(dbDisconnect(conn))
        
        rast <- tryCatch(
          fetch_raster(input$indice, input$temporalite, date_str, conn),
          error = function(e) NULL
        )
        
        if (is.null(rast)) return(data.frame(Message = "Aucune donnée disponible."))
        
        if (input$niveau != "National") {
          geom <- tryCatch(
            get_mask_geom(input$niveau, input$region_detail, input$province_detail,
                          input$commune_detail, input$region_filter,
                          input$region_commune_filter, input$province_commune_filter),
            error = function(e) NULL
          )
          if (!is.null(geom)) {
            geom_sp <- as(geom, "Spatial")
            rast    <- tryCatch(mask(rast, geom_sp), error = function(e) rast)
          }
        }
        
        config <- get_color_config(input$indice)
        vals   <- values(rast)
        vals   <- vals[!is.na(vals)]
        
        
        breaks <- config$breaks
        labels <- config$labels
        colors <- config$colors
        
        counts <- sapply(seq_along(labels), function(i) sum(vals >= breaks[i] & vals < breaks[i+1]))
        pcts   <- round(counts / length(vals) * 100, 2)
        
        # Single row dataframe with class percentages as columns
        df        <- as.data.frame(t(paste0(format(pcts, nsmall = 2), " %")))
        names(df) <- labels
        
        # Build colored header
        header_cells <- mapply(function(label, color) {
          htmltools::tags$th(
            label,
            style = paste0(
              "background-color:", color, ";",
              "color: black;",
              "text-align: center;",
              "border: 1px solid white;",
              "padding: 6px;"
            )
          )
        }, labels, colors, SIMPLIFY = FALSE)
        
        custom_header <- htmltools::tags$table(
          htmltools::tags$thead(
            htmltools::tags$tr(do.call(htmltools::tagList, header_cells))
          )
        )
        
        DT::datatable(
          df,
          container = custom_header,
          rownames  = FALSE,
          escape    = FALSE,
          options   = list(
            dom        = "t",
            ordering   = FALSE,
            pageLength = 1,
            scrollX    = TRUE,
            columnDefs = list(list(className = "dt-center", targets = "_all"))
          )
        )
      })
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