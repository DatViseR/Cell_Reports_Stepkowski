# This module will be used in different places in the app. It enables
# selection of GO categories from GO data uploaded in the main.R.
# It displays the selectsize input and update the UI accordingly when
# additional categories are added. On the server side it  saves the chosen_go categories
#in a reactive expression that can be used by other modules.


# Observe changes to the show_go_category input
observeEvent(input$show_go_category, {
  if (input$show_go_category) {
    cat( "GO categories selection enabled", "INFO from input$show_go_category")
  } else {
    cat("GO categories selection disabled", "INFO from input$show_go_category")
    # Clears the selectize input when disabling
    updateSelectizeInput(session, "go_category", selected = NULL)
  }
})

# Render the UI based on the toggle
output$go_category_ui <- renderUI({
  if (input$show_go_category) {
    selectizeInput("go_category", "Select from ~8000 unique GO categories", choices = NULL, multiple = TRUE)
  }
})

observe({
  if (input$show_go_category) {
    cat( "GO categories updated", "INFO from input$show_go_category")
    updateSelectizeInput(session, "go_category", choices = unique(GO$name), server = TRUE)
    
  }
})

# Reactive expression to track chosen GO categories
# chosen_go reactive returns NULL when GO categories are disabled
chosen_go <- reactive({
  # Only return GO categories if the feature is enabled
  if (!input$show_go_category) {
    cat("GO categories disabled - returning NULL", "INFO")
    return(NULL)
  }
  
  cat("Returning selected GO categories", "INFO")
  input$go_category
})






