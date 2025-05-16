box::use(
  shiny[moduleServer, NS, selectInput, textAreaInput, actionButton, div, h3, h4, p, br, 
        fluidRow, column, plotOutput, hr, conditionalPanel, tags, checkboxGroupInput, renderPlot, selectizeInput],
  bslib[card, card_header, card_body, layout_sidebar, sidebar, accordion, accordion_panel]
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  
  layout_sidebar(
    sidebar = sidebar(
      width = 300,
      title = "Analysis Options",
      
      # GO Category Selection
      accordion(
        open = TRUE,
        accordion_panel(
          "Select GO categories to highilight",
          toggle("show_go_category", "Visualize GO Categories", FALSE),
          # reactive UI module for the GO selection feuture
          GO_selection_module$uii(ns("go_selection_temporal")),
              ),
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
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Server logic would go here - for now just placeholders
    
    # color picker for GO
#    color_picker$server(id = ns("color_picker")) 
    GO_selection_module$server("go_selection_temporal")
    
    
    
    
    
    
    
    
    # Placeholder for volcano plots
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