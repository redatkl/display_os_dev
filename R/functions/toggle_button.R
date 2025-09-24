# Reusable toggle button troughout the application


# function to create a toggle switch
toggle_switch <- function(id, label = NULL) {
  tags$div(
    class = "toggle-wrapper",
    div(
      id = paste0(id, "_container"),
      class = "toggle-container",
      `data-toggle-id` = id,
      div(class = "toggle-button")
    ),
    if (!is.null(label)) {
      tags$label(label, class = "toggle-label")
    }
  )
}