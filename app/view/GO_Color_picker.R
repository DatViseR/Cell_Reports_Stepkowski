box::use(
  shiny[moduleServer, NS, renderUI, uiOutput, req, tagList, observe, reactive, 
        observeEvent, div, h4, p, tags, isolate],
  colourpicker[colourInput],
  htmltools[tagList]
)

# Define a default color palette to use when generating initial colors
DEFAULT_COLOR_PALETTE <- c(
  "#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", 
  "#FFFF33", "#A65628", "#F781BF", "#999999", "#66C2A5", 
  "#FC8D62", "#8DA0CB", "#E78AC3", "#A6D854", "#FFD92F"
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  
  div(
    h4("GO Category Colors"),
    p("Select colors for each GO category to use in visualizations:"),
    uiOutput(ns("color_picker_ui"))
  )
}

#' @export
server <- function(id, chosen_go = reactive(NULL)) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Store the current color assignments
    color_assignments <- reactive({
      req(chosen_go())
      chosen <- chosen_go()
      
      if (length(chosen) == 0) {
        return(NULL)
      }
      
      # Create a named vector of color assignments
      result <- sapply(seq_along(chosen), function(i) {
        go_term <- chosen[i]
        sanitized_id <- gsub("[^a-zA-Z0-9]", "_", go_term)
        input_name <- paste0("color_", sanitized_id)
        
        # Get the selected color from input, or assign a default if not yet set
        if (!is.null(input[[input_name]])) {
          return(input[[input_name]])
        } else {
          # Use modulo to cycle through the color palette
          return(DEFAULT_COLOR_PALETTE[(i - 1) %% length(DEFAULT_COLOR_PALETTE) + 1])
        }
      })
      
      names(result) <- chosen
      return(result)
    })
    
    # Render the UI based on chosen GO categories
    output$color_picker_ui <- renderUI({
      req(chosen_go())
      chosen <- chosen_go()
      
      if (length(chosen) == 0) {
        return(tags$div("No GO categories selected. Please select GO categories first."))
      }
      
      cat("Creating color pickers for GO categories:", paste(chosen, collapse = ", "), "\n")
      
      # Create a color picker for each GO category
      color_inputs <- lapply(seq_along(chosen), function(i) {
        go_term <- chosen[i]
        sanitized_id <- gsub("[^a-zA-Z0-9]", "_", go_term)
        color_value <- DEFAULT_COLOR_PALETTE[(i - 1) %% length(DEFAULT_COLOR_PALETTE) + 1]
        
        div(
          style = "margin-bottom: 10px;",
          colourInput(
            ns(paste0("color_", sanitized_id)), 
            label = tags$span(
              style = "font-size: 90%;", 
              paste("Color for", go_term)
            ),
            value = color_value
          )
        )
      })
      
      # Return all color pickers as a tagList
      do.call(tagList, color_inputs)
    })
    
    # Return the reactive color assignments
    return(list(
      go_colors = color_assignments
    ))
  })
}