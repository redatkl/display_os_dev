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
            div(class = "chart-title", uiOutput(ns("donut_title"))),
            div(
              class = "commune-list-wrapper",
              uiOutput(ns("commune_list"))
            )
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
    
    # Observe click
    observeEvent(input$selected_class, {
      selected_class(input$selected_class)
    })
    
    # ── Core data reactive: fetch + mask raster on search ─────────────────
    # ── SINGLE fetch on search: raster + all commune pixel values ──────────
    # Everything downstream reads from this — no re-fetching on badge click.
    base_data <- eventReactive(input$search, {
      req(input$indice, input$mois, input$annee, input$province_detail)
      
      # Reset selected class when new search starts
      selected_class(NULL)
      
      date_str <- build_date_str("mensuel", input$mois, NULL, input$annee)
      config   <- get_color_config(input$indice)
      
      # ── 1. Fetch raster ──────────────────────────────────────────────────
      rast <- tryCatch(
        fetch_raster(input$indice, "mensuel", date_str, conn),
        error = function(e) NULL
      )
      if (is.null(rast)) return(NULL)
      
      rast <- calc(rast, fun = function(x) {
        x[x < -9999 | is.infinite(x)] <- NA
        x
      })
      
      # ── 2. Province-level stats ──────────────────────────────────────────
      geom <- tryCatch(
        st_union(provinces[provinces$Nom_Provinces == input$province_detail, ]),
        error = function(e) NULL
      )
      rast_masked <- if (!is.null(geom)) {
        tryCatch(mask(rast, as(geom, "Spatial")), error = function(e) rast)
      } else rast
      
      prov_vals <- values(rast_masked)
      prov_vals <- prov_vals[!is.na(prov_vals)]
      
      breaks <- config$breaks
      labels <- config$labels
      colors <- config$colors
      
      counts <- sapply(seq_along(labels), function(i) {
        sum(prov_vals >= breaks[i] & prov_vals < breaks[i + 1])
      })
      pcts_province <- if (length(prov_vals) > 0) round(counts / length(prov_vals) * 100, 1) else rep(0, length(labels))
      
      is_drought <- grepl("Sécheresse|sécheresse", labels, ignore.case = TRUE)
      is_humid   <- grepl("humide|Humide|Amélioration", labels, ignore.case = TRUE)
      
      # ── 3. Extract pixel values per commune (done ONCE here) ─────────────
      prov_communes <- communes[
        !is.na(communes$Nom_Provinces) &
          communes$Nom_Provinces == input$province_detail,
      ]
      prov_communes <- sf::st_make_valid(prov_communes)
      prov_communes <- prov_communes[!sf::st_is_empty(prov_communes), ]
      
      commune_pixels <- NULL
      
      if (nrow(prov_communes) > 0) {
        # Extract list of pixel vectors — one entry per commune
        raw <- tryCatch(
          exactextractr::exact_extract(rast, prov_communes),
          error = function(e) NULL
        )
        
        if (!is.null(raw)) {
          commune_pixels <- lapply(seq_along(raw), function(j) {
            vals <- raw[[j]]$value
            vals[!is.na(vals)]
          })
          names(commune_pixels) <- prov_communes$commune
        }
      }
      
      list(
        config          = config,
        labels          = labels,
        colors          = colors,
        pcts_province   = pcts_province,
        drought         = sum(pcts_province[is_drought]),
        humid           = sum(pcts_province[is_humid]),
        normal          = sum(pcts_province[!is_drought & !is_humid]),
        indice          = input$indice,
        province        = input$province_detail,
        mois            = input$mois,
        annee           = input$annee,
        commune_pixels  = commune_pixels   # key: commune name, value: numeric vector
      )
    })
    
    # ── Commune ranking: pure computation from stored pixels ───────────────
    # No raster fetch here — just filters commune_pixels by class breaks.
    commune_ranking <- reactive({
      data <- base_data()
      sc   <- selected_class()
      req(data, sc, !is.null(data$commune_pixels))
      
      config    <- data$config
      class_idx <- which(config$labels == sc)
      if (length(class_idx) == 0) return(NULL)
      
      lo <- config$breaks[class_idx]
      hi <- config$breaks[class_idx + 1]
      
      pixels <- data$commune_pixels
      
      df <- data.frame(
        commune = names(pixels),
        pct     = sapply(pixels, function(vals) {
          if (length(vals) == 0) return(NA_real_)
          round(sum(vals >= lo & vals < hi) / length(vals) * 100, 1)
        }),
        stringsAsFactors = FALSE
      )
      
      df <- df[!is.na(df$pct), ]
      df[order(df$pct, decreasing = TRUE), ]
    })
    
    # ── Badges ─────────────────────────────────────────────────────────────
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
    
    # ── Left KPI: drought % ────────────────────────────────────────────────
    output$kpi_left_title <- renderUI({
      tags$p(paste0("Superficie en sécheresse (", input$indice, ")"))
    })
    
    output$kpi_left_value <- renderUI({
      data <- base_data()
      if (is.null(data)) {
        div(class = "kpi-badge kpi-gray", "— %")
      } else {
        cls <- if (data$drought >= 50) "kpi-red" else if (data$drought >= 25) "kpi-orange" else "kpi-yellow"
        div(class = paste("kpi-badge", cls), paste0(data$drought, " %"))
      }
    })
    
    output$kpi_left_sub <- renderUI({
      data <- base_data()
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
      data <- base_data()
      if (is.null(data)) {
        div(class = "kpi-badge kpi-gray", "— %")
      } else {
        cls <- if (data$humid >= 50) "kpi-blue" else if (data$humid >= 25) "kpi-teal" else "kpi-green"
        div(class = paste("kpi-badge", cls), paste0(data$humid, " %"))
      }
    })
    
    output$kpi_right_sub <- renderUI({
      data <- base_data()
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
    
    # ── Bottom-left chart: commune ranking ─────────────────────────────────
    output$donut_title <- renderUI({
      sc <- selected_class()
      
      div(
        style = "display: flex; justify-content: space-between; align-items: center; width: 100%;",
        
        # Left: always visible
        tags$span("Classement des communes", style = "font-size:13px; font-weight:600; color:#9ca3af;"),
        
        # Right: class indicator, only when a class is selected
        if (!is.null(sc)) {
          data      <- base_data()
          config    <- if (!is.null(data)) data$config else get_color_config(input$indice)
          class_idx <- which(config$labels == sc)
          dot_color <- if (length(class_idx) > 0) config$colors[class_idx] else "#888"
          
          div(
            style = "display: flex; align-items: center; gap: 5px;",
            tags$span(
              style = paste0(
                "display:inline-block; width:10px; height:10px;",
                "border-radius:50%; background:", dot_color, ";"
              )
            ),
            tags$span(sc, style = "font-size:12px; color:#d1d5db; max-width:180px; overflow:hidden; text-overflow:ellipsis; white-space:nowrap;")
          )
        }
      )
    })
    
    output$commune_list <- renderUI({
      sc <- selected_class()
      
      if (is.null(sc) || is.null(base_data())) {
        return(
          div(
            class = "commune-empty",
            tags$p("① Lancez une recherche"),
            tags$p("② Cliquez sur une classe")
          )
        )
      }
      
      df <- commune_ranking()
      
      if (is.null(df) || nrow(df) == 0) {
        return(div(class = "commune-empty", tags$p("Aucune donnée disponible")))
      }
      
      config    <- base_data()$config
      class_idx <- which(config$labels == sc)
      bar_color <- if (length(class_idx) > 0) config$colors[class_idx] else "#888888"
      
      # Text color on badge
      r <- strtoi(substr(bar_color, 2, 3), 16L)
      g <- strtoi(substr(bar_color, 4, 5), 16L)
      b <- strtoi(substr(bar_color, 6, 7), 16L)
      txt_col <- if ((r * 299 + g * 587 + b * 114) / 1000 < 128) "#ffffff" else "#1a1a1a"
      
      max_pct <- max(df$pct, na.rm = TRUE)
      if (max_pct == 0) max_pct <- 1   # avoid division by zero
      
      rows <- lapply(seq_len(nrow(df)), function(i) {
        rank <- i
        name <- df$commune[i]
        pct  <- df$pct[i]
        bar_w <- round(pct / max_pct * 100)
        
        div(
          class = "commune-row",
          # Rank number
          div(class = "commune-rank", paste0("#", rank)),
          # Name + progress bar
          div(
            class = "commune-info",
            div(class = "commune-name", name),
            div(
              class = "commune-bar-track",
              div(
                class = "commune-bar-fill",
                style = paste0(
                  "width:", bar_w, "%;",
                  "background-color:", bar_color, ";"
                )
              )
            )
          ),
          # Percentage badge
          div(
            class = "commune-pct",
            style = paste0(
              "background-color:", bar_color, ";",
              "color:", txt_col, ";"
            ),
            paste0(pct, "%")
          )
        )
      })
      
      do.call(tagList, rows)
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