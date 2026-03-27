# Bulletins modules
bulletinUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "css/bulletins_module.css")
    ),
    
    div(id = "reporting-container",
        # Sidebar for bulletin selection
        div(id = "reporting-sidebar",
            h3("Bulletins hebdomadaires", class = "mb-4"),
            
            # Filter section
            div(class = "filter-section",
                h4("Sélectionner un bulletin", class = "section-heading"),
                
                # Date input that suggests Mondays
                dateInput(ns("selected_date"), "Choisir une date:", 
                          value = NULL,
                          language = "fr",
                          min = "2020-01-01",
                          max = Sys.Date()),
                
                div(class = "calendar-note",
                    "Sélectionnez une date pour afficher le bulletin de la semaine correspondante."),
                
                # Current bulletin date range display
                uiOutput(ns("bulletin_info")),
                
                # Download button
                downloadButton(ns("download_pdf"), "Télécharger le PDF", 
                               class = "download-btn btn-block")
            ),
            
            # List of available bulletins
            h4("Archives disponibles", class = "section-heading mt-4"),
            uiOutput(ns("bulletin_list"))
        ),
        
        # Main content area displaying the PDF
        div(id = "reporting-content",
            uiOutput(ns("pdf_viewer"))
        )
    )
  )
}

# bulletins module server
bulletinsServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    
    # Helper function to parse a bulletin filename and extract dates
    parse_bulletin_filename <- function(filename) {
      # Remove file extension
      base_name <- tools::file_path_sans_ext(filename)
      
      # Try to parse the dd-dd_mm_yyyy format
      tryCatch({
        # Split by '_' first to get the date parts
        parts <- strsplit(base_name, "_")[[1]]
        
        if (length(parts) < 3) {
          return(NULL)  # Invalid format
        }
        
        # Get the day range, month, and year
        day_range <- parts[1]
        month <- parts[2]
        year <- parts[3]
        
        # Split day range into start and end
        day_parts <- strsplit(day_range, "-")[[1]]
        if (length(day_parts) < 2) {
          return(NULL)  # Invalid format
        }
        
        start_day <- as.numeric(day_parts[1])
        end_day <- as.numeric(day_parts[2])
        
        # Construct the dates
        end_date <- as.Date(paste0(year, "-", month, "-", end_day))
        
        # Calculate start date (might be in previous month)
        # If end_day < start_day, it means the start day is in the previous month
        if (end_day < start_day) {
          # Start date is in previous month
          prev_month_date <- end_date - end_day  # Go to end of previous month
          days_in_prev_month <- as.numeric(format(prev_month_date, "%d"))
          days_to_subtract <- days_in_prev_month - start_day
          start_date <- end_date - days_to_subtract - end_day
        } else {
          # Start date is in same month
          start_date <- end_date - (end_day - start_day)
        }
        
        # Return information about this bulletin
        return(list(
          file_name = filename,
          start_date = start_date,
          end_date = end_date,
          valid = TRUE
        ))
      }, error = function(e) {
        # If parsing fails, return NULL
        return(NULL)
      })
    }
    
    # Function to scan the directory and get all bulletin PDFs
    get_bulletins <- function() {
      # Ensure directory exists
      if (!dir.exists(pdf_base_path)) {
        warning("Bulletin directory does not exist: ", pdf_base_path)
        return(data.frame())
      }
      
      # List all PDF files
      pdf_files <- list.files(pdf_base_path, pattern = "\\.pdf$", ignore.case = TRUE)
      
      if (length(pdf_files) == 0) {
        return(data.frame())
      }
      
      # Parse each filename to extract date information
      bulletins <- lapply(pdf_files, function(file) {
        bulletin_info <- parse_bulletin_filename(file)
        
        if (is.null(bulletin_info)) {
          # Skip files that don't match our naming convention
          return(NULL)
        }
        
        # Return as a list with necessary information
        return(list(
          file_name = bulletin_info$file_name,
          start_date = bulletin_info$start_date,
          end_date = bulletin_info$end_date
        ))
      })
      
      # Remove NULL entries (files that couldn't be parsed)
      bulletins <- bulletins[!sapply(bulletins, is.null)]
      
      if (length(bulletins) == 0) {
        return(data.frame())
      }
      
      # Convert to data frame
      bulletins_df <- do.call(rbind.data.frame, lapply(bulletins, function(x) {
        data.frame(
          file_name = x$file_name,
          start_date = x$start_date, 
          end_date = x$end_date,
          stringsAsFactors = FALSE
        )
      }))
      
      # Add ID column based on start date (for easier selection)
      bulletins_df$id <- format(bulletins_df$start_date, "%Y-%m-%d")
      
      # Sort by end date (most recent first)
      bulletins_df <- bulletins_df[order(bulletins_df$end_date, decreasing = TRUE), ]
      
      return(bulletins_df)
    }
    
    # Load bulletins at startup
    bulletins <- reactiveVal(get_bulletins())
    
    # Display error message if no bulletins were found
    observe({
      if (nrow(bulletins()) == 0) {
        showNotification(
          "Aucun bulletin n'a été trouvé.",
          type = "warning",
          duration = 10
        )
      } else {
        # Set initial date to the most recent bulletin
        if (is.null(input$selected_date) && nrow(bulletins()) > 0) {
          # Get the start date of the most recent bulletin
          most_recent_date <- bulletins()[1, "start_date"]
          updateDateInput(session, "selected_date", value = most_recent_date)
        }
      }
    })
    
    # Function to find the bulletin that contains a given date
    find_bulletin_for_date <- function(date) {
      date <- as.Date(date)
      
      # Look for bulletins where the date falls within start_date to end_date
      matches <- bulletins()[
        bulletins()$start_date <= date & 
          bulletins()$end_date >= date, 
      ]
      
      if (nrow(matches) > 0) {
        # Return the first matching bulletin
        return(matches[1, ])
      }
      
      # If no direct match, find the closest bulletin by end date
      # (the most recent bulletin that ended before the selected date)
      past_bulletins <- bulletins()[bulletins()$end_date < date, ]
      
      if (nrow(past_bulletins) > 0) {
        # Return the most recent past bulletin
        return(past_bulletins[which.max(past_bulletins$end_date), ])
      }
      
      # If no past bulletins, try the earliest future bulletin
      future_bulletins <- bulletins()[bulletins()$start_date > date, ]
      
      if (nrow(future_bulletins) > 0) {
        # Return the earliest future bulletin
        return(future_bulletins[which.min(future_bulletins$start_date), ])
      }
      
      # No bulletins found
      return(NULL)
    }
    
    # Current selected bulletin
    selected_bulletin <- reactiveVal(NULL)
    
    # Update selected bulletin when date changes
    observeEvent(input$selected_date, {
      if (is.null(input$selected_date)) return()
      
      # Find bulletin for the selected date
      bulletin <- find_bulletin_for_date(input$selected_date)
      selected_bulletin(bulletin)
    })
    
    # Display information about the current bulletin
    output$bulletin_info <- renderUI({
      bulletin <- selected_bulletin()
      
      if (is.null(bulletin)) {
        return(NULL)
      }
      
      div(class = "date-info",
          strong("Bulletin sélectionné:"),
          p(paste(
            format(bulletin$start_date, "%d %b %Y"),
            "au",
            format(bulletin$end_date, "%d %b %Y")
          ))
      )
    })
    
    # Generate the list of available bulletins
    output$bulletin_list <- renderUI({
      ns <- session$ns
      
      # Get all bulletins, sorted by end date (descending)
      sorted_bulletins <- bulletins()
      
      if (nrow(sorted_bulletins) == 0) {
        return(div(class = "bulletin-archive",
                   p("Aucun bulletin disponible.")))
      }
      
      # Group bulletins by year and month for better organization
      bulletin_items <- list()
      
      # Current year and month for grouping
      current_year <- NULL
      current_month <- NULL
      
      for (i in 1:nrow(sorted_bulletins)) {
        bulletin <- sorted_bulletins[i, ]
        end_date <- bulletin$end_date
        
        # Get year and month for grouping
        bulletin_year <- as.numeric(format(end_date, "%Y"))
        bulletin_month <- as.numeric(format(end_date, "%m"))
        
        # Add year header if we've moved to a new year
        if (is.null(current_year) || current_year != bulletin_year) {
          current_year <- bulletin_year
          current_month <- NULL
          bulletin_items[[length(bulletin_items) + 1]] <- div(
            h5(current_year, style = "margin-top: 15px; font-weight: bold;")
          )
        }
        
        # Add month header if we've moved to a new month
        if (is.null(current_month) || current_month != bulletin_month) {
          current_month <- bulletin_month
          bulletin_items[[length(bulletin_items) + 1]] <- div(
            h6(month.name[current_month], style = "margin-top: 10px; color: #666;")
          )
        }
        
        # Format dates for display
        start_day <- format(bulletin$start_date, "%d")
        end_day <- format(bulletin$end_date, "%d")
        month_abbr <- format(bulletin$end_date, "%b")
        
        display_text <- paste(start_day, "-", end_day, month_abbr)
        
        # See if this bulletin is currently selected
        is_selected <- !is.null(selected_bulletin()) && 
          !is.null(selected_bulletin()$id) &&
          selected_bulletin()$id == bulletin$id
        
        # Create the bulletin list item
        bulletin_items[[length(bulletin_items) + 1]] <- div(
          class = paste("archive-item", if(is_selected) "active" else ""),
          id = ns(paste0("bulletin_item_", bulletin$id)),
          onclick = sprintf("Shiny.setInputValue('%s', '%s', {priority: 'event'});",
                            ns("select_bulletin"), bulletin$id),
          
          display_text
        )
      }
      
      # Wrap all items in the bulletin-archive div
      div(class = "bulletin-archive",
          do.call(tagList, bulletin_items))
    })
    
    # Handle bulletin selection from list
    observeEvent(input$select_bulletin, {
      bulletin_id <- input$select_bulletin
      selected <- bulletins()[bulletins()$id == bulletin_id, ]
      
      if (nrow(selected) > 0) {
        # Update date input to match selected bulletin
        updateDateInput(session, "selected_date", value = selected$start_date)
        
        # Update selected bulletin
        selected_bulletin(selected)
      }
    })
    
    # Render PDF viewer
    output$pdf_viewer <- renderUI({
      bulletin <- selected_bulletin()
      
      if (is.null(bulletin)) {
        return(div(class = "no-pdf-message",
                   h4("Aucun bulletin disponible"),
                   p("Veuillez sélectionner une autre date.")))
      }
      
      # Construct PDF path
      pdf_path <- file.path(pdf_base_path, bulletin$file_name)
      
      # Check if file exists
      if (!file.exists(pdf_path)) {
        return(div(class = "bulletin-error",
                   h4("Fichier PDF introuvable"),
                   p(paste("Le bulletin", bulletin$file_name, "n'existe pas dans le répertoire.")),
                   p("Vérifiez que le fichier est bien présent dans le dossier des bulletins.")))
      }
      
      # Display PDF using an iframe with improved height handling
      div(style = "height: 100%; display: flex; flex-direction: column;",
          tags$iframe(
            class = "pdf-viewer",
            src = paste0("bulletins/", bulletin$file_name),
            frameborder = "0"
          )
      )
    })
    
    # Download handler for the PDF
    output$download_pdf <- downloadHandler(
      filename = function() {
        bulletin <- selected_bulletin()
        if (is.null(bulletin)) {
          return("bulletin.pdf")
        }
        bulletin$file_name
      },
      content = function(file) {
        bulletin <- selected_bulletin()
        if (is.null(bulletin)) {
          # If no bulletin is selected, provide an empty PDF
          pdf(file, width = 8.5, height = 11)
          plot.new()
          text(0.5, 0.5, "No bulletin selected")
          dev.off()
          return()
        }
        
        # Get the source file path
        src_file <- file.path(pdf_base_path, bulletin$file_name)
        
        # Check if source file exists
        if (!file.exists(src_file)) {
          # Create an error PDF if file doesn't exist
          pdf(file, width = 8.5, height = 11)
          plot.new()
          text(0.5, 0.8, "Error: Bulletin file not found")
          text(0.5, 0.5, src_file)
          dev.off()
          return()
        }
        
        # Copy the actual file
        file.copy(src_file, file)
      }
    )
    
    # Function to refresh the bulletins list (could be called by a button)
    refresh_bulletins <- function() {
      # Reload bulletins from directory
      new_bulletins <- get_bulletins()
      bulletins(new_bulletins)
      
      # Update selected bulletin if needed
      if (!is.null(input$selected_date)) {
        bulletin <- find_bulletin_for_date(input$selected_date)
        selected_bulletin(bulletin)
      }
    }
  })
}