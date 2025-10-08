# datepicker.R - Year/Month Picker

# Custom year-month picker input function
customDatePickerInput <- function(inputId, label = NULL, value = Sys.Date()) {
  # Format value as YYYY-MM
  formatted_value <- format(value, "%B %Y")
  
  div(
    class = "form-group",
    if (!is.null(label)) tags$label(label, `for` = inputId),
    div(
      class = "dp-container",
      tags$input(
        id = inputId,
        class = "form-control dp-input",
        type = "text",
        readonly = "readonly",
        value = formatted_value
      ),
      div(
        class = "dp-popup",
        id = paste0(inputId, "_popup"),
        div(
          class = "dp-header",
          tags$button(class = "dp-nav dp-prev", "‹"),
          div(class = "dp-month"),
          tags$button(class = "dp-nav dp-next", "›")
        ),
        div(class = "dp-grid")
      )
    ),
    tags$script(HTML(sprintf(
      "$(document).ready(function() { new DatePicker('%s'); });",
      inputId
    )))
  )
}