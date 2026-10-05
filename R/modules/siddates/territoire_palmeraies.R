# SIDDATES 2026 > Territoire et Palmeraies

# Provinces of the action zones (oasis, argan, and common zones)
zone_action_path <- "data/sidattes/zone_action/zone_action_ANDZOA.geojson"
zone_action <- st_read(zone_action_path, quiet = TRUE)

# Serve the zones file to the browser (the 3D card loads it with D3)
addResourcePath("sidattes_zone_action", "data/sidattes/zone_action")

# Derive the zone type from the two boolean flags of the file
zone_action$zone <- dplyr::case_when(
  zone_action$zone_dattes == "TRUE" & zone_action$zone_commune_datt_argan_txt == "TRUE" ~ "Zone commune (Palmier dattier et Arganier)",
  zone_action$zone_dattes == "TRUE" ~ "Zone Oasienne",
  TRUE ~ "Zone Arganier"
)

zone_levels <- c("Zone Oasienne", "Zone Arganier", "Zone commune (Palmier dattier et Arganier)")
zone_colors <- c("#791617", "#156433", "#7a7de4")
zone_pal <- colorFactor(zone_colors, domain = zone_levels)

territoire_palmeraies_ui <- function(id) {
  ns <- NS(id)

  tagList(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "css/territoire_palmeraies.css"),
      tags$script(src = "https://d3js.org/d3.v7.min.js"),
      tags$script(src = "js/territoire_3d.js")
    ),

    tags$div(
      class = "territoire-row",

      # Card with Morocco in grey and the zones as a 3D layer
      tags$div(
        class = "territoire-card",
        tags$h4(class = "territoire-card-title", icon("location-dot"), "Zone d'action ANDZOA"),
        tags$svg(
          id = ns("zone_3d"),
          class = "territoire-3d-svg",
          viewBox = "0 0 600 700",
          preserveAspectRatio = "xMidYMid meet",
          `data-zones-url` = "sidattes_zone_action/zone_action_ANDZOA.geojson",
          `data-morocco-url` = "sidattes_zone_action/Maroc.geojson",
          `data-levels` = jsonlite::toJSON(zone_levels),
          `data-colors` = jsonlite::toJSON(zone_colors)
        ),
        tags$div(
          class = "territoire-legend",
          lapply(seq_along(zone_levels), function(i) {
            tags$div(
              class = "territoire-legend-item",
              tags$span(class = "territoire-legend-dot", style = paste0("background:", zone_colors[i])),
              zone_levels[i]
            )
          })
        )
      ),

      # Key figures of the date palm sector
      tags$div(
        class = "territoire-kpis",

        # 1. Superficie
        kpi_card("Superficie", "fa-seedling", kpi_series(
          c("2008/09", "50 900 ha"), c("2021/22", "63 000 ha"), c("2025/26", "69 490 ha")
        ), "Sources : communiqué officiel SIDATTES, 2022 ; FAO, 2025"),

        # 2. Production
        kpi_card("Production", "fa-boxes-stacked", kpi_series(
          c("2008/09", "90 400 t"), c("2020/21", "149 000 t"), c("2025/26", "160 000 t")
        ), "Sources : communiqué officiel SIDATTES, 2022 ; FAO, 2025"),

        # 3. Chiffre d'affaires
        kpi_card("Chiffre d'affaires", "fa-coins",
                 tags$div(class = "kpi-big", "2 milliards", tags$span("DH"))),

        # 4. Journées de travail
        kpi_card("Emploi", "fa-people-group",
                 tags$div(class = "kpi-big", "3,6 millions", tags$span("journées de travail"))),

        # 5. Stratégie Génération Green
        kpi_card("Stratégie Génération Green (SGG)", "fa-leaf", tags$ul(
          class = "kpi-list",
          tags$li("Plantation de ", tags$b("5 millions"), " de jeunes arbres"),
          tags$li("Dont ", tags$b("3 millions"), " dans la palmeraie traditionnelle"),
          tags$li("Production de ", tags$b("300 000 t/an"), " d'ici 2030")
        ), class = "kpi-wide")
      )
    )
  )
}

# One KPI card: title, icon, body and optional sources footer
kpi_card <- function(title, icon_name, body, source = NULL, class = NULL) {
  tags$div(
    class = paste("kpi-card", class),
    tags$div(class = "kpi-head", icon(sub("^fa-", "", icon_name)), tags$span(title)),
    body,
    if (!is.null(source)) tags$div(class = "kpi-source", source)
  )
}

# Rows of "period -> value" for the evolution cards
kpi_series <- function(...) {
  tags$div(
    class = "kpi-series",
    lapply(list(...), function(x) {
      tags$div(class = "kpi-row", tags$span(class = "kpi-period", x[1]), tags$span(class = "kpi-value", x[2]))
    })
  )
}

territoire_palmeraies_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    # Draw the 3D card (id is namespaced, so pass the full DOM id)
    session$onFlushed(function() {
      session$sendCustomMessage("territoire3d", list(id = session$ns("zone_3d")))
    }, once = TRUE)

  })
}