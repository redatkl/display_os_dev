# Salons > SIDDATES 2026 page module
# Tab bar on top, one module per tab

source("R/modules/siddates/territoire_palmeraies.R")
source("R/modules/siddates/climat_secheresse.R")
source("R/modules/siddates/etat_vegetation.R")
source("R/modules/siddates/eau_ressources.R")
source("R/modules/siddates/sol.R")
source("R/modules/siddates/organisations_pro.R")
source("R/modules/siddates/startups.R")

# id = tab value, label = text shown in the tab bar
siddates_tabs <- list(
  list(id = "territoire", label = "Territoire et Palmeraies"),
  list(id = "climat",     label = "Climat & Sécheresse"),
  list(id = "vegetation", label = "Etat de la végétation"),
  list(id = "eau",        label = "Eau et Ressources"),
  list(id = "sol",        label = "Sol"),
  list(id = "orgs",       label = "Organisations professionnelles"),
  list(id = "startups",   label = "Start-ups")
)

siddates_ui <- function(id) {
  ns <- NS(id)

  tagList(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "css/siddates.css")
    ),

    # Tab bar
    tags$div(
      class = "siddates-tabs",
      lapply(seq_along(siddates_tabs), function(i) {
        tab <- siddates_tabs[[i]]
        tags$button(
          class = paste("siddates-tab", if (i == 1) "active"),
          onclick = sprintf(
            "Shiny.setInputValue('%s', '%s'); $('.siddates-tab').removeClass('active'); $(this).addClass('active');",
            ns("tab"), tab$id
          ),
          tab$label
        )
      })
    ),

    # Tab content (first tab shown until a tab is clicked)
    tags$div(
      class = "siddates-content",
      conditionalPanel(
        condition = "typeof input.tab === 'undefined' || input.tab == 'territoire'",
        ns = ns,
        territoire_palmeraies_ui(ns("territoire"))
      ),
      conditionalPanel(
        condition = "input.tab == 'climat'", ns = ns,
        climat_secheresse_ui(ns("climat"))
      ),
      conditionalPanel(
        condition = "input.tab == 'vegetation'", ns = ns,
        etat_vegetation_ui(ns("vegetation"))
      ),
      conditionalPanel(
        condition = "input.tab == 'eau'", ns = ns,
        eau_ressources_ui(ns("eau"))
      ),
      conditionalPanel(
        condition = "input.tab == 'sol'", ns = ns,
        sol_ui(ns("sol"))
      ),
      conditionalPanel(
        condition = "input.tab == 'orgs'", ns = ns,
        organisations_pro_ui(ns("orgs"))
      ),
      conditionalPanel(
        condition = "input.tab == 'startups'", ns = ns,
        startups_ui(ns("startups"))
      )
    )
  )
}

siddates_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    territoire_palmeraies_server("territoire")
    climat_secheresse_server("climat")
    etat_vegetation_server("vegetation")
    eau_ressources_server("eau")
    sol_server("sol")
    organisations_pro_server("orgs")
    startups_server("startups")
  })
}