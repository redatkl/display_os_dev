# Reusable panel component for sidebar panels
# Helper function to create panel content
create_panel_content <- function(ns, panel_id, indices_options, indices_label = "Choix d'indice") {
  tagList(
    # Map selector (conditionally shown) - UNIQUE per panel
    conditionalPanel(
      condition = "input.map_layout_selected == 'layout2' || input.map_layout_selected == 'layout4'",
      ns = ns,
      div(
        class = "map-selector-container",
        div(class = "map-selector-header",
            icon("map"), 
            span("Carte active")
        ),
        uiOutput(ns(paste0("map_selector_ui_", panel_id)))
      )
    ),
    
    # choix de la temporalité des indices - UNIQUE per panel
    div(
      id = ns(paste0("temporalite_", panel_id)),
      class = "temporalite-section",
      div(class = "header",
          icon("calendar"), 
          span("Choix de temporalité")
      ),
      div(class = "temporalite-container",
          toggle_switch_group(
            group_id = ns(paste0("filter_options_", panel_id)),
            options = list(
              "decadaire" = "Décadaire",
              "mensuel" = "Mensuel", 
              "trimestriel" = "Trimestriel",
              "annuel" = "Annuel"
            ),
            selected = "mensuel",
            disabled = c("decadaire")
          )
      ),
      div(class = "date-input",
          customDatePickerInput(ns(paste0("custom_date_", panel_id)), value = Sys.Date())
      )
    ),
    
    # choix d'indice - CUSTOMIZABLE SECTION
    div(
      id = ns(paste0("indice_", panel_id)),
      class = "indice-section",
      div(class = "indice-header",
          icon("database"), 
          span(indices_label)
      ),
      div(class = "indice-container",
          toggle_switch_group(
            group_id = ns(paste0("filter_", panel_id, "_options")),
            options = indices_options,
            selected = names(indices_options)[1]
          )
      )
    ),
    
    # la zone de la légende - UNIQUE per panel
    div(
      id = ns(paste0("legend_", panel_id)),
      class = "legend-section",
      div(class = "legend-header",
          icon("circle-info"), 
          span("Légende")
      ),
      div(class = "legend-container",
          # tags$img(src = "logos/logo.png", 
          #          height = "100px",
          #          width = "150px",
          #          alt = "Légende des indices", 
          #          class = "legend-image")
          
          # Dynamic legend output
          uiOutput(ns(paste0("legend_ui_", panel_id)))
      )
    ),
    
    # Update the map button
    div(
      class = "update-button-container",
      actionButton(
        ns(paste0("update_map_", panel_id)),
        label = "Mettre à jour la carte",
        class = "btn-update-map",
        icon = icon("refresh")
      )
    )
  )
}