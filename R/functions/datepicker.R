# reusable datepicker trough out the shiny app
# datepicker.R

# Custom datepicker input function
customDatePickerInput <- function(inputId, label = NULL, value = Sys.Date()) {

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
          value = format(value, "%Y-%m-%d")
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