library(shiny)
library(bslib)

ui <- page_fluid(
  theme = bs_theme(version = 5),
  
  # Custom CSS for the toggle switch
  tags$head(
    tags$style(HTML("
      .toggle-container {
        position: relative;
        width: 100px;
        height: 40px;
        background-color: white;
        border-radius: 4px;
        cursor: pointer;
        transition: background-color 0.3s ease;
        margin: 20px 0;
      }
      
      .toggle-container.active {
        background-color: #4CAF50;
      }
      
      .toggle-button {
        position: absolute;
        top: 2px;
        left: 2px;
        width: 36px;
        height: 36px;
        background-color: #ccc;
        border-radius: 4px;
        transition: transform 0.3s ease;
        box-shadow: 0 2px 4px rgba(0,0,0,0.2);
      }
      
      .toggle-button.active {
        transform: translateX(60px);
      }
      
      .demo-section {
        padding: 30px;
        text-align: center;
      }
    "))
  ),
  
  div(class = "demo-section",
      h2("Custom Toggle Switch"),
      p("Click the toggle to see it move from left to right and change color"),
      
      # Custom toggle switch
      div(
        id = "toggle_container",
        class = "toggle-container",
        div(class = "toggle-button")
      ),
      
      br(),
      
      # Display current state
      card(
        card_header("Toggle State"),
        textOutput("toggle_status")
      )
  ),
  
  # JavaScript for toggle functionality
  tags$script(HTML("
    $(document).ready(function() {
      let isActive = false;
      
      $('#toggle_container').click(function() {
        isActive = !isActive;
        
        if (isActive) {
          $(this).addClass('active');
          $(this).find('.toggle-button').addClass('active');
        } else {
          $(this).removeClass('active');
          $(this).find('.toggle-button').removeClass('active');
        }
        
        // Send state to Shiny server
        Shiny.setInputValue('toggle_state', isActive);
      });
    });
  "))
)

server <- function(input, output, session) {
  
  # Initialize toggle state
  observe({
    if (is.null(input$toggle_state)) {
      # Set initial state
      session$sendCustomMessage("initToggle", FALSE)
    }
  })
  
  # Display toggle status
  output$toggle_status <- renderText({
    state <- input$toggle_state
    if (is.null(state) || !state) {
      "Toggle is OFF (left position, gray background)"
    } else {
      "Toggle is ON (right position, green background)"
    }
  })
}

shinyApp(ui = ui, server = server)