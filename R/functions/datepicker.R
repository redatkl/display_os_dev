# datepicker.R - Year/Month Picker

# Helper: format a Date as French "mois année" string (locale-independent)
current_date_fr <- function(date = Sys.Date()) {
  months_fr <- c("janvier", "f\u00e9vrier", "mars", "avril", "mai", "juin",
                 "juillet", "ao\u00fbt", "septembre", "octobre", "novembre", "d\u00e9cembre")
  paste(months_fr[as.integer(format(date, "%m"))], format(date, "%Y"))
}

# Custom year-month picker input function
customDatePickerInput <- function(inputId, label = NULL, value = Sys.Date()) {
  # Format value as YYYY-MM
  formatted_value <- current_date_fr(value)
  
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