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
            div(class = "bubble-label", "Classifications"),
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
            div(class = "chart-title", "Répartition des conditions"),
            uiOutput(ns("donut_chart"))
          ),
          div(
            class = "chart-card evolution-card",
            div(class = "chart-title", "Évolution — 3 derniers mois"),
            uiOutput(ns("evolution_chart"))
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
      selected_class(NULL)
      
      config <- get_color_config(input$indice)
      
      # ── Helper: fetch + mask raster for a given month/year ──────────────────
      fetch_month_pcts <- function(m, y) {
        ds   <- build_date_str("mensuel", m, NULL, y)
        rast <- tryCatch(fetch_raster(input$indice, "mensuel", ds, conn), error = function(e) NULL)
        if (is.null(rast)) return(NULL)
        
        rast <- calc(rast, fun = function(x) { x[x < -9999 | is.infinite(x)] <- NA; x })
        
        geom <- tryCatch(st_union(provinces[provinces$Nom_Provinces == input$province_detail, ]), error = function(e) NULL)
        if (!is.null(geom)) rast <- tryCatch(mask(rast, as(geom, "Spatial")), error = function(e) rast)
        
        vals <- values(rast); vals <- vals[!is.na(vals)]
        if (length(vals) == 0) return(NULL)
        
        counts <- sapply(seq_along(config$labels), function(i) {
          sum(vals >= config$breaks[i] & vals < config$breaks[i + 1])
        })
        round(counts / length(vals) * 100, 1)
      }
      
      # ── Build last 3 months list ─────────────────────────────────────────────
      months_seq <- lapply(2:0, function(offset) {
        m <- as.integer(input$mois) - offset
        y <- as.integer(input$annee)
        while (m <= 0) { m <- m + 12; y <- y - 1 }
        list(m = m, y = y)
      })
      
      months_fr <- c("jan","fév","mar","avr","mai","jun","jul","aoû","sep","oct","nov","déc")
      
      evolution <- lapply(months_seq, function(my) {
        pcts <- fetch_month_pcts(my$m, my$y)
        list(
          label = paste0(months_fr[my$m], "\n", my$y),
          pcts  = pcts
        )
      })
      
      # ── Current month full data (same as before) ─────────────────────────────
      pcts_province <- evolution[[3]]$pcts %||% rep(0, length(config$labels))
      
      is_drought <- grepl("Sécheresse|sécheresse", config$labels, ignore.case = TRUE)
      is_humid   <- grepl("humide|Humide|Amélioration", config$labels, ignore.case = TRUE)
      
      # ── Commune pixels ────────────────────────────────────────────────────────
      # Re-fetch current month raster for commune extraction
      rast_cur <- tryCatch(
        fetch_raster(input$indice, "mensuel",
                     build_date_str("mensuel", input$mois, NULL, input$annee), conn),
        error = function(e) NULL
      )
      if (!is.null(rast_cur)) {
        rast_cur <- calc(rast_cur, fun = function(x) { x[x < -9999 | is.infinite(x)] <- NA; x })
      }
      
      prov_communes <- communes[!is.na(communes$Nom_Provinces) & communes$Nom_Provinces == input$province_detail, ]
      prov_communes <- sf::st_make_valid(prov_communes)
      prov_communes <- prov_communes[!sf::st_is_empty(prov_communes), ]
      
      commune_pixels <- NULL
      if (!is.null(rast_cur) && nrow(prov_communes) > 0) {
        raw <- tryCatch(exactextractr::exact_extract(rast_cur, prov_communes), error = function(e) NULL)
        if (!is.null(raw)) {
          commune_pixels <- lapply(seq_along(raw), function(j) {
            vals <- raw[[j]]$value; vals[!is.na(vals)]
          })
          names(commune_pixels) <- prov_communes$commune
        }
      }
      
      list(
        config         = config,
        labels         = config$labels,
        colors         = config$colors,
        pcts_province  = pcts_province,
        drought        = sum(pcts_province[is_drought]),
        humid          = sum(pcts_province[is_humid]),
        normal         = sum(pcts_province[!is_drought & !is_humid]),
        indice         = input$indice,
        province       = input$province_detail,
        mois           = input$mois,
        annee          = input$annee,
        commune_pixels = commune_pixels,
        evolution      = evolution      
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
        tags$span("Classement des communes les plus touchées:", style = "font-size:13px; font-weight:600; color:#9ca3af;"),
        
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
    
    
    # donut chart ----------------------------------------------------------
    output$donut_chart <- renderUI({
      data <- base_data()
      
      if (is.null(data)) {
        return(div(class = "donut-empty", tags$p("Lancez une recherche")))
      }
      
      config <- data$config
      labels <- config$labels
      colors <- config$colors
      pcts   <- data$pcts_province
      
      keep_idx <- which(pcts > 0)
      labels_k <- labels[keep_idx]
      colors_k <- colors[keep_idx]
      pcts_k   <- pcts[keep_idx]
      
      if (length(pcts_k) == 0) {
        return(div(class = "donut-empty", tags$p("Aucune donnée disponible")))
      }
      
      # ── Build conic-gradient stops ───────────────────────────────────────────
      total   <- sum(pcts_k)
      cumul   <- cumsum(pcts_k / total * 100)
      starts  <- c(0, cumul[-length(cumul)])
      
      stops <- paste(
        sapply(seq_along(pcts_k), function(i) {
          paste0(colors_k[i], " ", round(starts[i], 2), "% ", round(cumul[i], 2), "%")
        }),
        collapse = ", "
      )
      
      conic <- paste0("conic-gradient(", stops, ")")
      
      # ── Legend items ─────────────────────────────────────────────────────────
      legend_items <- lapply(seq_along(labels_k), function(i) {
        div(
          class = "donut-legend-item",
          div(class = "donut-legend-dot",
              style = paste0("background:", colors_k[i], ";")),
          div(
            class = "donut-legend-text",
            tags$span(class = "donut-legend-label",
                      badge_short(labels_k[i], keep_idx[i])),
            tags$span(class = "donut-legend-pct",
                      paste0(pcts_k[i], "%"))
          )
        )
      })
      
      # ── Assemble ─────────────────────────────────────────────────────────────
      div(
        class = "donut-wrapper",
        
        # Donut circle
        div(
          class = "donut-container",
          div(class = "donut-ring",
              style = paste0("background:", conic, ";")),
          div(
            class = "donut-hole",
            div(class = "donut-center-province", data$province),
            div(class = "donut-center-meta",
                paste0(data$indice, " — ", data$mois, "/", data$annee))
          )
        ),
        
        # Legend
        div(class = "donut-legend", do.call(tagList, legend_items))
      )
    })
    
    output$evolution_chart <- renderUI({
      data <- base_data()
      
      if (is.null(data)) {
        return(div(class = "donut-empty", tags$p("Lancez une recherche")))
      }
      
      evolution <- data$evolution
      colors    <- data$colors
      labels    <- data$labels
      
      bars <- lapply(evolution, function(month) {
        pcts <- month$pcts
        lbl  <- month$label
        
        if (is.null(pcts)) {
          segments <- div(
            style = "flex:1; background:#374151; display:flex; align-items:center; justify-content:center;",
            tags$span("N/D", style = "color:#6b7280; font-size:10px;")
          )
        } else {
          segments <- lapply(seq_along(labels), function(i) {
            if (pcts[i] <= 0) return(NULL)
            # text color
            r <- strtoi(substr(colors[i], 2, 3), 16L)
            g <- strtoi(substr(colors[i], 4, 5), 16L)
            b <- strtoi(substr(colors[i], 6, 7), 16L)
            tc <- if ((r*299 + g*587 + b*114)/1000 < 128) "#fff" else "#111"
            
            div(
              style = paste0(
                "flex:", pcts[i], "%;",
                "background-color:", colors[i], ";",
                "display:flex; align-items:center; justify-content:center;",
                "overflow:hidden; transition: height 0.4s ease;",
                "min-height:", if (pcts[i] >= 5) "0" else "0", "px;"
              ),
              title = paste0(badge_short(labels[i], i), ": ", pcts[i], "%"),
              if (pcts[i] >= 8) tags$span(
                paste0(pcts[i], "%"),
                style = paste0("font-size:9px; font-weight:700; color:", tc, ";")
              )
            )
          })
          segments <- Filter(Negate(is.null), segments)
        }
        
        div(
          class = "evol-bar-col",
          div(class = "evol-bar-stack", do.call(tagList, if (is.list(segments)) segments else list(segments))),
          div(class = "evol-bar-label", gsub("\n", " ", lbl))
        )
      })
      
      div(
        class = "evol-wrapper",
        # Y-axis labels
        div(
          class = "evol-yaxis",
          lapply(c(100, 75, 50, 25, 0), function(v) {
            div(class = "evol-ytick", paste0(v, "%"))
          })
        ),
        # Bars
        div(class = "evol-bars", do.call(tagList, bars))
      )
    })
    
    observeEvent(input$search, {
      message("Dashboard search triggered: indice=", input$indice,
              " niveau=", input$niveau, " mois=", input$mois,
              " annee=", input$annee)
      return(base_data())
    })
  })
}