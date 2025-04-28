box::use(
  shiny[moduleServer, NS, selectInput, textAreaInput, actionButton, div, h3, h4, p, br, 
        fluidRow, column, plotOutput, hr, conditionalPanel, tags, checkboxGroupInput],
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
          "Gene Selection",
          selectInput(
            ns("selection_type"),
            "Selection Type",
            choices = c("GO Category", "Custom Genes"),
            selected = "GO Category"
          ),
          
          # GO Category selection
          conditionalPanel(
            condition = "input.selection_type == 'GO Category'", 
            ns = ns,
            selectInput(
              ns("go_category"),
              "GO Category",
              choices = c(
                "Immune response" = "GO:0006955",
                "Inflammatory response" = "GO:0006954",
                "Cell proliferation" = "GO:0008283",
                "Apoptotic process" = "GO:0006915",
                "Response to stress" = "GO:0006950",
                "Cell adhesion" = "GO:0007155"
              ),
              selected = "GO:0006955"
            ),
            
            # Additional filters for GO category
            checkboxGroupInput(
              ns("go_subcategories"),
              "Subcategories",
              choices = c(
                "Cytokine signaling" = "cytokine",
                "T cell activation" = "t_cell",
                "B cell activation" = "b_cell",
                "NK cell function" = "nk_cell"
              )
            )
          ),
          
          # Custom gene selection
          conditionalPanel(
            condition = "input.selection_type == 'Custom Genes'",
            ns = ns,
            textAreaInput(
              ns("custom_genes"),
              "Enter Gene Names",
              placeholder = "Enter gene names separated by commas or new lines (e.g., IL6, TNF, IFNG)",
              rows = 5
            ),
            actionButton(
              ns("load_example_genes"),
              "Load Example Genes",
              class = "btn-outline-info"
            )
          ),
          
          hr(),
          actionButton(
            ns("apply_selection"),
            "Apply Selection",
            class = "btn-primary",
            width = "100%"
          )
        ),
        
        accordion_panel(
          "Display Options",
          checkboxGroupInput(
            ns("time_points"),
            "Time Points to Display",
            choices = c("2h", "6h", "12h", "24h"),
            selected = c("2h", "6h", "12h", "24h")
          ),
          selectInput(
            ns("significance_threshold"),
            "Significance Threshold",
            choices = c("p < 0.05", "p < 0.01", "p < 0.001"),
            selected = "p < 0.05"
          ),
          selectInput(
            ns("fold_change_threshold"),
            "Fold Change Threshold",
            choices = c("FC > 1.5", "FC > 2.0", "FC > 3.0"),
            selected = "FC > 2.0"
          )
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
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Server logic would go here - for now just placeholders
    
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