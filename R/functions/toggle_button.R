# Reusable toggle button troughout the application

# Group toggle switch function
toggle_switch_group <- function(group_id, options, selected = NULL, label = NULL) {
  
  # Create individual toggles
  toggles <- lapply(names(options), function(key) {
    option_id <- paste0(group_id, "_", key)
    is_selected <- !is.null(selected) && selected == key
    
    tags$div(
      class = "toggle-item",
      tags$div(
        class = "toggle-wrapper",
        div(
          id = paste0(option_id, "_container"),
          class = if(is_selected) "toggle-container active" else "toggle-container",
          `data-toggle-id` = option_id,
          `data-group-id` = group_id,
          `data-option-key` = key,
          div(class = if(is_selected) "toggle-button active" else "toggle-button")
        ),
        tags$label(options[[key]], class = "toggle-label")
      )
    )
  })
  
  # Wrap in group container
  tags$div(
    class = "toggle-group",
    `data-group-id` = group_id,
    if (!is.null(label)) {
      tags$div(class = "toggle-group-label", label)
    },
    toggles
  )
}