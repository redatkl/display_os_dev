# SIDDATES 2026 > Climat & Sécheresse
# Left: 3D map of the ANDZOA provinces (click = select). Right: graphs of the selection.
# Needs the echarts4r package.

library(echarts4r)

climat_provinces <- st_read("data/sidattes/zone_action/zone_action_ANDZOA.geojson", quiet = TRUE)

# Served by territoire_palmeraies.R (same folder), repeated here so the module is standalone
addResourcePath("sidattes_zone_action", "data/sidattes/zone_action")

# Annual precipitation (mm) averaged over a geometry, computed live in PostGIS.
# Monthly pixel means inside the polygon are summed per year; years with fewer
# than 12 months (the current one) are dropped.
fetch_precip_annual <- function(conn, geom) {
  wkt <- st_as_text(st_union(st_geometry(geom)))

  dbGetQuery(conn, "
    SELECT year, SUM(m) AS precip
    FROM (
      SELECT year, month,
             (ST_SummaryStats(ST_Clip(rast, 1, ST_GeomFromText($1, 4326), true))).mean AS m
      FROM public.morocco_chirps
      WHERE ST_Intersects(rast, ST_GeomFromText($1, 4326))
    ) t
    WHERE m IS NOT NULL
    GROUP BY year
    HAVING COUNT(*) = 12
    ORDER BY year
  ", params = list(wkt))
}

climat_secheresse_ui <- function(id) {
  ns <- NS(id)

  tagList(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "css/climat_secheresse.css"),
      tags$script(src = "https://d3js.org/d3.v7.min.js"),
      tags$script(src = "js/climat_3d.js")
    ),

    tags$div(
      class = "climat-row",

      # Left: clickable 3D map
      tags$div(
        class = "climat-card climat-map-card",
        tags$h4(class = "climat-card-title", icon("location-dot"), "Choisir une province"),
        tags$svg(
          id = ns("zone_map"),
          class = "climat-3d-svg",
          viewBox = "0 0 600 700",
          preserveAspectRatio = "xMidYMid meet",
          `data-zones-url` = "sidattes_zone_action/zone_action_ANDZOA.geojson",
          `data-morocco-url` = "sidattes_zone_action/Maroc.geojson",
          `data-input` = ns("province")
        ),
        tags$div(class = "climat-hint", "Cliquez sur une province, cliquez à nouveau pour revenir à l'ensemble")
      ),

      # Right: graphs
      tags$div(
        class = "climat-panels",
        tags$div(
          class = "climat-card",
          tags$h4(class = "climat-card-title", textOutput(ns("title"), inline = TRUE)),
          uiOutput(ns("stats")),
          shinycssloaders::withSpinner(
            echarts4rOutput(ns("annual_chart"), height = "380px"),
            color = "#047857"
          )
        )
      )
    )
  )
}

climat_secheresse_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    conn <- init_db()
    onStop(function() dbDisconnect(conn))

    # Draw the map once the page is ready
    session$onFlushed(function() {
      session$sendCustomMessage("climat3d", list(id = session$ns("zone_map")))
    }, once = TRUE)

    # Selected province code (NULL = all ANDZOA provinces)
    selected <- reactive({
      if (is.null(input$province) || input$province == "") NULL else input$province
    })

    selected_name <- reactive({
      if (is.null(selected())) return("Ensemble des provinces ANDZOA")
      climat_provinces$Nom_Provinces[climat_provinces$Code_Province == selected()][1]
    })

    # Live query, cached per selection
    precip <- reactive({
      code <- selected()
      geom <- if (is.null(code)) climat_provinces else climat_provinces[climat_provinces$Code_Province == code, ]
      fetch_precip_annual(conn, geom)
    }) %>% bindCache(selected())

    output$title <- renderText({
      paste0("Précipitations annuelles - ", selected_name())
    })

    # Summary tiles
    output$stats <- renderUI({
      d <- precip()
      req(nrow(d) > 0)

      trend <- unname(coef(lm(precip ~ year, data = d))[2]) * 10
      tile <- function(label, value, sub = NULL) {
        tags$div(
          class = "climat-tile",
          tags$div(class = "climat-tile-label", label),
          tags$div(class = "climat-tile-value", value),
          if (!is.null(sub)) tags$div(class = "climat-tile-sub", sub)
        )
      }

      tags$div(
        class = "climat-tiles",
        tile("Moyenne annuelle", paste(round(mean(d$precip)), "mm"), paste0(min(d$year), "-", max(d$year))),
        tile("Année la plus humide", paste(round(max(d$precip)), "mm"), d$year[which.max(d$precip)]),
        tile("Année la plus sèche", paste(round(min(d$precip)), "mm"), d$year[which.min(d$precip)]),
        tile("Tendance", sprintf("%+.0f mm", trend), "par décennie")
      )
    })

    # Annual precipitation: bars vs. the long-term mean + 5-year moving average
    output$annual_chart <- renderEcharts4r({
      d <- precip()
      req(nrow(d) > 0)

      m <- mean(d$precip)
      d$year <- as.character(d$year)
      d$above <- ifelse(d$precip >= m, round(d$precip), NA)
      d$below <- ifelse(d$precip < m, round(d$precip), NA)
      d$ma <- round(as.numeric(stats::filter(d$precip, rep(1 / 5, 5), sides = 1)))

      d %>%
        e_charts(year) %>%
        e_bar(above, name = "Au-dessus de la moyenne", stack = "p",
              itemStyle = list(color = "#2b83ba", borderRadius = c(3, 3, 0, 0))) %>%
        e_bar(below, name = "En dessous de la moyenne", stack = "p",
              itemStyle = list(color = "#c2793a", borderRadius = c(3, 3, 0, 0))) %>%
        e_line(ma, name = "Moyenne mobile 5 ans", symbol = "none",
               lineStyle = list(width = 3, color = "#156433")) %>%
        e_mark_line(data = list(yAxis = round(m), name = "Moyenne"), symbol = "none",
                    lineStyle = list(type = "dashed", color = "#791617"),
                    label = list(formatter = paste0("Moyenne : ", round(m), " mm"))) %>%
        e_tooltip(trigger = "axis") %>%
        e_y_axis(name = "mm", nameLocation = "end") %>%
        e_x_axis(axisLabel = list(interval = 4)) %>%
        e_legend(bottom = 0) %>%
        e_grid(left = 50, right = 20, top = 40, bottom = 50)
    })

  })
}