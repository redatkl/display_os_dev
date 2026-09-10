# Reusable panel component sidebar for the station page
# Helper function to create panel content
create_station_panel_content <- function(ns, panel_id, options = NULL, selected = NULL, label = NULL, disabled_options = NULL, include_temporalite = FALSE, date_value = Sys.Date(), include_button = TRUE) {
  tagList(
    
    if (!is.null(options)) {
    # choix de la source de données - UNIQUE per panel
    div(
        id = ns(paste0("options_", panel_id)),
        class = "indice-section",
        div(class = "indice-header",
            icon("database"),
            span(label)
        ),
        div(class = "indice-container",
            toggle_switch_group(
              group_id = ns(paste0("filter_", panel_id, "_options")),
              options = options,
              selected = selected,
              disabled = disabled_options
            )
        )
      )
    },
    
    # ── Temporalité section (Choix de temporalité panel) ───────────────────────
    if (include_temporalite) {
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
                "decadaire"   = "Décadaire",
                "mensuel"     = "Mensuel",
                "trimestriel" = "Trimestriel",
                "annuel"      = "Annuel"
              ),
              selected = "mensuel",
              disabled = c("decadaire")
            )
        ),
        div(class = "date-input",
            customDatePickerInput(ns(paste0("custom_date_", panel_id)), value = date_value)
        )
      )
    },

    if (include_button) {        # ── Only when include button is TRUE ─────────────────
    # ── Update button (one per panel) ─────────────────
    div(
      class = "update-button-container",
      actionButton(
        ns(paste0("update_", panel_id)),
        label = "Mettre à jour stations",
        class = "btn-update-station",
        icon = icon("refresh")
      )
    )
    }
  )
}