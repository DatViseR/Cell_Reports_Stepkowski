box::use(
  shiny[moduleServer, NS, selectInput, textAreaInput, actionButton, div, h3, h4, p, br, 
        fluidRow, column, plotOutput, hr, conditionalPanel, tags, checkboxGroupInput, renderPlot, selectizeInput, observe],
  bslib[card, card_header, card_body, layout_sidebar, sidebar, accordion, accordion_panel],
  graphics[text],
  # Import the GO_selection_module
  app/view/GO_selection_module,
  app/view/GO_Color_picker
)

#' @export
ui <- function(id, GO = NULL) {
  ns <- NS(id)
  
  layout_sidebar(
    sidebar = sidebar(
      width = 300,
      title = "Analysis Options",
      
      # GO Category Selection
      accordion(
        open = TRUE,
        accordion_panel(
          "GO Categories",
          # Implement GO_selection_module UI
          GO_selection_module$ui(ns("go_selection_temporal")),
          GO_Color_picker$ui(ns("go_color_picker"))
        )
      )
    ),
    
    # Main content
    div(
      h3("Temporal Gene Expression Alterations", 
         style = "color: #0062cc; margin-bottom: 20px;"),
      
      # Volcano plots - 2x2 grid
      card(
        card_header(
          h4("Differential Expression Across Time Points", style = "margin: 0;")
        ),
        card_body(
          p("Volcano plots showing differential expression at each time point:"),
          br(),
          fluidRow(
            column(
              width = 6,
              plotOutput(ns("volcano_2h"), height = "300px"),
              div(tags$b("2 hours"), style = "text-align: center; margin-top: 5px;")
            ),
            column(
              width = 6,
              plotOutput(ns("volcano_6h"), height = "300px"),
              div(tags$b("6 hours"), style = "text-align: center; margin-top: 5px;")
            )
          ),
          br(),
          fluidRow(
            column(
              width = 6,
              plotOutput(ns("volcano_12h"), height = "300px"),
              div(tags$b("12 hours"), style = "text-align: center; margin-top: 5px;")
            ),
            column(
              width = 6,
              plotOutput(ns("volcano_24h"), height = "300px"),
              div(tags$b("24 hours"), style = "text-align: center; margin-top: 5px;")
            )
          )
        )
      ),
      
      br(),
      
      # Heatmap
      card(
        card_header(
          h4("Expression Heatmap Across Time Points", style = "margin: 0;")
        ),
        card_body(
          p("Heatmap showing temporal expression patterns:"),
          br(),
          plotOutput(ns("temporal_heatmap"), height = "500px"),
          br(),
          div(
            style = "font-size: 90%; color: #666;",
            "Rows represent genes, columns represent time points (2h, 6h, 12h, 24h). 
            Color scale indicates log2 fold change relative to control."
          )
        )
      )
    )
  )
}

#' @export
server <- function(id, GO= NULL) {
  moduleServer(id, function(input, output, session) {
    # Get the GO data from parent scope to pass to GO_selection_module
    # This will be available from main.R
    
    # Initialize the GO selection module and get the reactive values
    go_selection <- GO_selection_module$server("go_selection_temporal", GO = GO)
    
    
    # Initialize the color picker module with the chosen GO categories
    go_colors <- GO_Color_picker$server("go_color_picker", chosen_go = go_selection$chosen_go)
    
    # Observe selected GO categories for visualization
    observe({
      selected_go <- go_selection$chosen_go()
      if (!is.null(selected_go)) {
        cat("Selected GO categories:", paste(selected_go, collapse=", "), "\n")
        # This data can now be used in plots
      }
    })
    
    # Placeholder for volcano plots - can be updated to use selected GO categories
    output$volcano_2h <- renderPlot({
      # Placeholder plot
      plot(1:10, 1:10, type = "n", xlab = "log2 Fold Change", ylab = "-log10(p-value)",
           main = "2h Volcano Plot (Placeholder)")
      text(5, 5, "Volcano Plot\nPlaceholder", cex = 2)
    })
    
    output$volcano_6h <- renderPlot({
      # Placeholder plot
      plot(1:10, 1:10, type = "n", xlab = "log2 Fold Change", ylab = "-log10(p-value)",
           main = "6h Volcano Plot (Placeholder)")
      text(5, 5, "Volcano Plot\nPlaceholder", cex = 2)
    })
    
    output$volcano_12h <- renderPlot({
      # Placeholder plot
      plot(1:10, 1:10, type = "n", xlab = "log2 Fold Change", ylab = "-log10(p-value)",
           main = "12h Volcano Plot (Placeholder)")
      text(5, 5, "Volcano Plot\nPlaceholder", cex = 2)
    })
    
    output$volcano_24h <- renderPlot({
      # Placeholder plot
      plot(1:10, 1:10, type = "n", xlab = "log2 Fold Change", ylab = "-log10(p-value)",
           main = "24h Volcano Plot (Placeholder)")
      text(5, 5, "Volcano Plot\nPlaceholder", cex = 2)
    })
    
    # Placeholder for heatmap
    output$temporal_heatmap <- renderPlot({
      # Placeholder plot
      plot(1:10, 1:10, type = "n", xlab = "Time Points", ylab = "Genes",
           main = "Temporal Expression Heatmap (Placeholder)")
      text(5, 5, "Heatmap\nPlaceholder", cex = 3)
    })
  })
}