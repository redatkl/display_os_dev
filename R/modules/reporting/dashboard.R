source("R/functions/db_config.R")

# R/modules/reporting/dashboard.R

# Shorten a class label for display inside the badge circle
badge_short <- function(label, index) {
  # Use index-based short codes that match indice semantics
  shorts <- c("D6", "D5", "D4", "N", "H4", "H5", "H6")
  if (index <= length(shorts)) shorts[[index]] else paste0("C", index)
}

dashboard_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "css/dashboard.css")
    ),
    
    div(
      class = "dashboard-wrapper",
      
      # ── Control bar ────────────────────────────────────────
      div(
        class = "dashboard-control-bar",
        
        div(class = "control-group",
            tags$span("Indice", class = "control-label"),
            selectInput(ns("indice"), label = NULL,
                        choices = c("SPI" = "SPI", "ANDVI" = "ANDVI", "CDI" = "CDI"),
                        selected = "SPI", width = "100px")
        ),
        
        div(class = "control-group",
            tags$span("Niveau", class = "control-label"),
            disabled(
            selectInput(ns("niveau"), label = NULL,
                        choices = c("Provincial"),
                        selected = "Provincial", width = "120px")
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
                )))
        ),
        
        div(class = "control-group",
            tags$span("Mois", class = "control-label"),
            selectInput(ns("mois"), label = NULL,
                        choices = c("Janvier"=1,"Février"=2,"Mars"=3,
                                    "Avril"=4,"Mai"=5,"Juin"=6,
                                    "Juillet"=7,"Août"=8,"Septembre"=9,
                                    "Octobre"=10,"Novembre"=11,"Décembre"=12),
                        selected = 3, width = "100px")
        ),
        
        div(class = "control-group",
            tags$span("Année", class = "control-label"),
            selectInput(ns("annee"), label = NULL,
                        choices = seq(2000, as.integer(format(Sys.Date(), "%Y"))),
                        selected = as.integer(format(Sys.Date(), "%Y")),
                        width = "80px")
        ),
        
        actionButton(ns("search"), label = NULL,
                     icon = icon("magnifying-glass"),
                     class = "btn search-btn")
      ),
      
      # ── Main dashboard area ────────────────────────────────
      div(
        class = "dashboard-main",
        
        # ── Row 1: KPI cards + bubble chart ─────────────────
        div(
          class = "dashboard-row row-1",
          
          # KPI card left
          div(
            class = "kpi-card",
            id    = ns("kpi_left"),
            div(class = "kpi-title", uiOutput(ns("kpi_left_title"))),
            div(class = "kpi-value", uiOutput(ns("kpi_left_value"))),
            div(class = "kpi-subtitle", uiOutput(ns("kpi_left_sub")))
          ),
          
          # Center: bubble/badge chart
          div(
            class = "bubble-card",
            div(class = "bubble-label", "Les plus touchées:"),
            div(class = "bubble-area",  uiOutput(ns("bubble_badges"))),
            div(class = "bubble-meta",  uiOutput(ns("bubble_meta")))
          ),
          
          # KPI card right
          div(
            class = "kpi-card",
            id    = ns("kpi_right"),
            div(class = "kpi-title", uiOutput(ns("kpi_right_title"))),
            div(class = "kpi-value", uiOutput(ns("kpi_right_value"))),
            div(class = "kpi-subtitle", uiOutput(ns("kpi_right_sub")))
          )
        ),
        
        # ── Row 2: Donut + Bar chart ─────────────────────────
        div(
          class = "dashboard-row row-2",
          
          # Donut chart
          div(
            class = "chart-card donut-card",
            div(class = "chart-title", "Répartition par classe"),
            plotOutput(ns("donut_chart"), height = "280px")
          ),
          
          # Bar chart
          div(
            class = "chart-card bar-card",
            div(class = "chart-title", "Évolution temporelle"),
            plotOutput(ns("bar_chart"), height = "280px")
          )
        )
      )
    )
  )
}

dashboard_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    conn <- init_db()
    
    # Communes filtered by province
    update_provinces <- function(region_name) {
      req(region_name, nchar(region_name) > 0)
      code_region <- paste0(sprintf("%02d", regions$id_region[regions$nom_fr == region_name]), ".")
      filtered_provinces <- na.omit(provinces$Nom_Provinces[provinces$Code_Region == code_region])
      updateSelectInput(session, "province_detail", choices  = filtered_provinces, selected = filtered_provinces[1])
    }
    
    # Province Observers
    observeEvent(input$region_filter, {
      if (input$niveau == "Provincial") {
        update_provinces(input$region_filter)
      }
    })
    
    # Track selected class
    selected_class <- reactiveVal(NULL)
    
    output$bubble_badges <- renderUI({
      req(input$indice)
      config <- get_color_config(input$indice)
      
      if (is.null(config)) return(NULL)
      
      div(
        class = "badge-row",
        lapply(seq_along(config$labels), function(i) {
          label  <- config$labels[[i]]
          color  <- config$colors[[i]]
          
          # Decide text color based on background brightness
          # dark backgrounds → white text, light → dark text
          is_dark <- grepl("^#[0-9A-Fa-f]{6}$", color) && {
            r <- strtoi(substr(color, 2, 3), 16L)
            g <- strtoi(substr(color, 4, 5), 16L)
            b <- strtoi(substr(color, 6, 7), 16L)
            (r * 299 + g * 587 + b * 114) / 1000 < 128
          }
          text_color <- if (is_dark) "#ffffff" else "#1a1a1a"
          
          is_selected <- !is.null(selected_class()) && selected_class() == label
          
          border_style <- if (is_selected)
            "border: 3px solid #ffffff; box-shadow: 0 0 10px rgba(255,255,255,0.6);"
          else
            "border: 3px solid transparent;"
          
          div(
            class = "drought-badge",
            style = paste0(
              "background-color:", color, ";",
              "color:", text_color, ";",
              border_style,
              "cursor: pointer;"
            ),
            title = label,  # tooltip on hover
            # Short label: last word or abbreviated
            onclick = sprintf(
              "Shiny.setInputValue('%s', '%s', {priority:'event'})",
              ns("selected_class"),
              gsub("'", "\\\\'", label)
            ),
            # Display short version
            div(class = "badge-short",  badge_short(label, i)),
            div(class = "badge-index",  paste0("C", i))
          )
        })
      )
    })
    
    # Observe click
    observeEvent(input$selected_class, {
      selected_class(input$selected_class)
    })
    
    # ── Core data reactive: fetch + mask raster on search ─────────────────
    dashboard_data <- eventReactive(input$search, {
      req(input$indice, input$mois, input$annee, input$province_detail)
      
      date_str <- build_date_str("mensuel", input$mois, NULL, input$annee)
      
      rast <- tryCatch(
        fetch_raster(input$indice, "mensuel", date_str, conn),
        error = function(e) NULL
      )
      
      if (is.null(rast)) return(NULL)
      
      # Clean NoData
      rast <- calc(rast, fun = function(x) {
        x[x < -9999 | is.infinite(x)] <- NA
        x
      })
      
      # Mask to selected province
      geom <- tryCatch(
        st_union(provinces[provinces$Nom_Provinces == input$province_detail, ]),
        error = function(e) NULL
      )
      if (!is.null(geom)) {
        rast <- tryCatch(mask(rast, as(geom, "Spatial")), error = function(e) rast)
      }
      
      config <- get_color_config(input$indice)
      vals   <- values(rast)
      vals   <- vals[!is.na(vals)]
      
      if (length(vals) == 0) return(NULL)
      
      breaks <- config$breaks
      labels <- config$labels
      colors <- config$colors
      
      counts <- sapply(seq_along(labels), function(i) {
        sum(vals >= breaks[i] & vals < breaks[i + 1])
      })
      pcts <- round(counts / length(vals) * 100, 1)
      
      # Tag each class as drought / normal / humid
      is_drought <- grepl("Sécheresse|sécheresse", labels, ignore.case = TRUE)
      is_humid   <- grepl("humide|Humide|Amélioration", labels, ignore.case = TRUE)
      
      list(
        labels    = labels,
        colors    = colors,
        pcts      = pcts,
        drought   = sum(pcts[is_drought]),
        humid     = sum(pcts[is_humid]),
        normal    = sum(pcts[!is_drought & !is_humid]),
        indice    = input$indice,
        province  = input$province_detail,
        mois      = input$mois,
        annee     = input$annee
      )
    })
    
    # ── Left KPI: drought % ────────────────────────────────────────────────
    output$kpi_left_title <- renderUI({
      tags$p(paste0("Superficie en sécheresse (", input$indice, ")"))
    })
    
    output$kpi_left_value <- renderUI({
      data <- dashboard_data()
      if (is.null(data)) {
        div(class = "kpi-badge kpi-gray", "— %")
      } else {
        cls <- if (data$drought >= 50) "kpi-red" else if (data$drought >= 25) "kpi-orange" else "kpi-yellow"
        div(class = paste("kpi-badge", cls), paste0(data$drought, " %"))
      }
    })
    
    output$kpi_left_sub <- renderUI({
      data <- dashboard_data()
      if (is.null(data)) return(tags$p("Lancez une recherche"))
      tagList(
        tags$p(paste0("Province : ", data$province)),
        tags$p(paste0("Période : ", data$mois, "/", data$annee))
      )
    })
    
    # ── Right KPI: humid % ─────────────────────────────────────────────────
    output$kpi_right_title <- renderUI({
      tags$p(paste0("Superficie en humidité (", input$indice, ")"))
    })
    
    output$kpi_right_value <- renderUI({
      data <- dashboard_data()
      if (is.null(data)) {
        div(class = "kpi-badge kpi-gray", "— %")
      } else {
        cls <- if (data$humid >= 50) "kpi-blue" else if (data$humid >= 25) "kpi-teal" else "kpi-green"
        div(class = paste("kpi-badge", cls), paste0(data$humid, " %"))
      }
    })
    
    output$kpi_right_sub <- renderUI({
      data <- dashboard_data()
      if (is.null(data)) return(tags$p("Lancez une recherche"))
      tagList(
        tags$p(paste0("Province : ", data$province)),
        tags$p(paste0("Normal : ", data$normal, " %"))
      )
    })
    
    # ── Meta ───────────────────────────────────────────────────────────────
    output$bubble_meta <- renderUI({
      tagList(
        tags$p(paste0("Mois: ", input$mois)),
        tags$p(paste0("Indice: ", tolower(input$indice)))
      )
    })
    
    output$donut_chart <- renderPlot({
      par(bg = "#1a1a2e", fg = "white")
      plot.new()
      text(0.5, 0.5, "Donut chart\n(à implémenter)", col = "white", cex = 1.2)
    })
    
    output$bar_chart <- renderPlot({
      par(bg = "#1a1a2e", fg = "white")
      plot.new()
      text(0.5, 0.5, "Bar chart\n(à implémenter)", col = "white", cex = 1.2)
    })
    
    observeEvent(input$search, {
      message("Dashboard search triggered: indice=", input$indice,
              " niveau=", input$niveau, " mois=", input$mois,
              " annee=", input$annee)
    })
  })
}