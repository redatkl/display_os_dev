# Reusable toggle button troughout the application

# Group toggle switch function
toggle_switch_group <- function(group_id, options, selected = NULL, label = NULL, disabled = NULL) {
  
  # Create individual toggles
  toggles <- lapply(names(options), function(key) {
    option_id <- paste0(group_id, "_", key)
    is_selected <- !is.null(selected) && selected == key
    is_disabled <- !is.null(disabled) && key %in% disabled
    
    # Determine classes
    container_class <- "toggle-container"
    button_class <- "toggle-button"
    label_class <- "toggle-label"
    
    if (is_disabled) {
      container_class <- paste(container_class, "disabled")
      label_class <- paste(label_class, "disabled")
    } else if (is_selected) {
      container_class <- paste(container_class, "active")
      button_class <- paste(button_class, "active")
    }
    
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
          `data-disabled` = if(is_disabled) "true" else "false",
          div(class = button_class)
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