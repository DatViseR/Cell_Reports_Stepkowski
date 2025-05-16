# This module will be used in different places in the app. It enables
# selection of GO categories from GO data uploaded in the main.R.
# It displays the selectsize input and update the UI accordingly when
# additional categories are added. On the server side it  saves the chosen_go categories
#in a reactive expression that can be used by other modules.


box::use(
shiny[moduleServer, NS, selectizeInput, updateSelectizeInput, renderUI, uiOutput,
      observeEvent, reactive, observe, req, isolate],
bslib[card, card_body],
htmltools[div, tags]
)

#' @export
uii <- function(id) {
  ns <- NS(id)
  
  div(
    uiOutput(ns("go_category_ui"))
  )
}

#' @export
ui <- function(id) {
  ns <- NS(id)
  
  div(
    tags$label("GO Categories Selection", class = "control-label"),
    tags$div(
      tags$input(
        id = ns("show_go_category"), 
        type = "checkbox", 
        class = "form-check-input"
      ),
      tags$label(
        "Visualize GO Categories", 
        class = "form-check-label",
        `for` = ns("show_go_category")
      )
    ),
    uiOutput(ns("go_category_ui"))
  )
}

#' @export
server <- function(id, GO = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Observe changes to the show_go_category input
    observeEvent(input$show_go_category, {
      if (input$show_go_category) {
        cat("GO categories selection enabled\n")
      } else {
        cat("GO categories selection disabled\n")
        # Clears the selectize input when disabling
        updateSelectizeInput(session, "go_category", selected = NULL)
      }
    })
    
    # Render the UI based on the toggle
    output$go_category_ui <- renderUI({
      if (input$show_go_category) {
        selectizeInput(ns("go_category"), "Select from ~8000 unique GO categories", 
                       choices = NULL, 
                       multiple = TRUE,
                       options = list(placeholder = 'Start typing to search GO terms...'))
      }
    })
    
    # Update selectize input with GO categories
    observe({
      req(input$show_go_category)
      
      
      cat("Updating GO categories list\n")
      updateSelectizeInput(session, "go_category", 
                           choices = unique(GO$name), 
                           server = TRUE)
    })
    
  
    # Reactive expression to track chosen GO categories
    chosen_go <- reactive({
      if (input$show_go_category) {
        req(input$go_category)
        cat("Chosen GO categories:", paste(input$go_category, collapse=", "), "\n")
        return(input$go_category)
      } else {
        return(NULL)
      }
    })
    
    # Return the chosen GO categories
    return(list(
      chosen_go = chosen_go
    ))
   
  }
  )
}