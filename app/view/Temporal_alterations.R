box::use(
  shiny[moduleServer, NS, selectInput, textAreaInput, actionButton, div, h3, h4, p, br, 
        fluidRow, column, plotOutput, hr, conditionalPanel, tags, checkboxGroupInput, renderPlot, selectizeInput, observe],
  bslib[card, card_header, card_body, layout_sidebar, sidebar, accordion, accordion_panel, input_switch],
  graphics[text],
  # Import the GO_selection_module
  app/view/GO_selection_module,
  app/view/GO_Color_picker,
  app/view/gene_annotation_input,
  app/view/temporal_volcano_grid,
  app/logic/go_helpers
)

#' @export
ui <- function(id, GO = NULL) {
  ns <- NS(id)
  
  layout_sidebar(
    sidebar = sidebar(
      width = 350,
      title = "Analysis Options",
      
      # Analysis modules in accordion
      accordion(
        open = TRUE,
        
        # GO Category Selection
        GO_selection_module$ui(ns("go_selection_temporal")),
        
        # Gene Annotation Input
        gene_annotation_input$ui(ns("gene_annotation")),
        
        # Color picker for GO categories
        GO_Color_picker$ui(ns("go_color_picker"))
      )
    ),
    
    # Main content
    div(
      h3("Temporal Gene Expression Alterations - Dataset I", 
         style = "color: #0062cc; margin-bottom: 20px;"),
      
      p("Analysis of differential gene expression during CCCP treatment and recovery phases using Dataset I temporal data."),
      
      # Temporal volcano plots grid
      temporal_volcano_grid$ui(ns("temporal_volcanoes")),
      
      br(),
      
      # Heatmap placeholder (can be implemented later)
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
            "Rows represent genes, columns represent time points (STRESS I, STRESS II, RECOVERY I, RECOVERY II). 
            Color scale indicates log2 fold change relative to control."
          )
        )
      )
    )
  )
}

#' @export
server <- function(id, GO = NULL, dataset_1 = NULL) {
  moduleServer(id, function(input, output, session) {
    # Initialize the GO selection module and get the reactive values
    go_selection <- GO_selection_module$server("go_selection_temporal", GO = GO)
    
    # Initialize the gene annotation input module
    gene_annotation <- gene_annotation_input$server("gene_annotation")
    
    # Initialize the color picker module with the chosen GO categories
    go_colors <- GO_Color_picker$server("go_color_picker", chosen_go = go_selection$chosen_go)
    
    # Extract genes from selected GO categories
    go_genes <- reactive({
      selected_go <- go_selection$chosen_go()
      if (!is.null(selected_go) && !is.null(GO)) {
        go_helpers$get_genes_from_go_categories(GO, selected_go)
      } else {
        NULL
      }
    })
    
    # Get custom genes from annotation input
    custom_genes <- reactive({
      gene_annotation$selected_genes()
    })
    
    # Observe selected GO categories for debugging
    observe({
      selected_go <- go_selection$chosen_go()
      go_genes_list <- go_genes()
      custom_genes_list <- custom_genes()
      
      if (!is.null(selected_go)) {
        cat("Selected GO categories:", paste(selected_go, collapse=", "), "\n")
        if (!is.null(go_genes_list)) {
          cat("GO genes count:", length(go_genes_list), "\n")
        }
      }
      
      if (!is.null(custom_genes_list)) {
        cat("Custom genes count:", length(custom_genes_list), "\n")
      }
    })
    
    # Initialize the temporal volcano grid with all data
    temporal_volcanoes <- temporal_volcano_grid$server(
      "temporal_volcanoes",
      dataset_1 = reactive(dataset_1),
      go_genes = go_genes,
      custom_genes = custom_genes,
      fc_cutoff = reactive(1),
      pval_cutoff = reactive(0.05)
    )
    
    # Placeholder for heatmap - can be implemented later
    output$temporal_heatmap <- renderPlot({
      # Placeholder plot
      plot(1:10, 1:10, type = "n", xlab = "Time Points", ylab = "Genes",
           main = "Temporal Expression Heatmap (Placeholder)")
      text(5, 5, "Heatmap\nPlaceholder\n(Future Implementation)", cex = 2)
    })
  })
}