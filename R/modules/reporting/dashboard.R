# R/modules/reporting/dashboard.R

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
            selectInput(ns("niveau"), label = NULL,
                        choices = c("National", "Régional", "Provincial"),
                        selected = "Provincial", width = "120px")
        ),
        
        div(class = "control-group",
            tags$span("Province", class = "control-label"),
            selectInput(ns("province"), label = NULL,
                        choices = c("Settat"),
                        selected = "Settat", width = "120px")
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
    
    # ── Placeholder outputs (fill in with real data later) ──
    
    output$kpi_left_title <- renderUI({
      tags$p(paste0("Superficie D_4 (", tolower(input$indice), ") en %"))
    })
    output$kpi_left_value <- renderUI({
      div(class = "kpi-badge kpi-yellow", "— %")
    })
    output$kpi_left_sub <- renderUI({
      tags$p("Niveau : S. Modérée")
      tags$p(paste0("Mois : ", input$mois))
    })
    
    output$kpi_right_title <- renderUI({
      tags$p("Superficie D_6 (andvi) en %")
    })
    output$kpi_right_value <- renderUI({
      div(class = "kpi-badge kpi-red", "— %")
    })
    output$kpi_right_sub <- renderUI({
      tags$p("Niveau : S. Extrême")
    })
    
    output$bubble_badges <- renderUI({
      div(class = "badge-row",
          lapply(0:6, function(i) {
            cls <- c("b-blue","b-blue","b-blue","b-gray",
                     "b-yellow","b-orange","b-red")[i+1]
            div(class = paste("drought-badge", cls), paste0("D_", i))
          })
      )
    })
    
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