# This module provides gene annotation input functionality
# allowing users to manually input gene names or upload a file with gene names

box::use(
  shiny[moduleServer, NS, textAreaInput, fileInput, actionButton, div, h4, p, br,
        reactive, observeEvent, req, tags, selectInput, checkboxInput, renderUI, uiOutput],
  bslib[card, card_header, card_body, accordion, accordion_panel],
  htmltools[tagList],
  utils[read.table, read.csv]
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  
  accordion(
    accordion_panel(
      title = "Gene Annotation Input",
      icon = "fas fa-tag",
      
      div(
        p("Add custom gene annotations to highlight specific genes in volcano plots."),
        
        # Method selection
        selectInput(
          ns("input_method"),
          "Select input method:",
          choices = list(
            "Manual input" = "manual",
            "Upload file" = "upload"
          ),
          selected = "manual"
        ),
        
        # Conditional UI based on method
        uiOutput(ns("input_ui")),
        
        br(),
        
        # Action button
        actionButton(
          ns("apply_genes"),
          "Apply Gene Annotations",
          class = "btn-primary",
          style = "width: 100%;"
        ),
        
        br(), br(),
        
        # Display current genes
        uiOutput(ns("current_genes_display"))
      )
    )
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Store the current gene list
    current_genes <- reactive({
      list()
    })
    
    # Reactive to store processed genes
    gene_list <- reactive({
      NULL
    })
    
    # Render conditional UI based on input method
    output$input_ui <- renderUI({
      req(input$input_method)
      
      if (input$input_method == "manual") {
        tagList(
          textAreaInput(
            ns("manual_genes"),
            "Enter gene names (one per line or comma-separated):",
            placeholder = "Enter gene names here...\nExample:\nTP53\nEGFR\nBRCA1",
            rows = 6,
            width = "100%"
          ),
          div(
            style = "font-size: 90%; color: #666; margin-top: 5px;",
            "Enter gene names separated by new lines or commas"
          )
        )
      } else {
        tagList(
          fileInput(
            ns("gene_file"),
            "Upload gene list file:",
            accept = c(".txt", ".csv", ".tsv"),
            width = "100%"
          ),
          div(
            style = "font-size: 90%; color: #666; margin-top: 5px;",
            "Supported formats: .txt, .csv, .tsv (one gene per line or first column)"
          )
        )
      }
    })
    
    # Process uploaded file
    uploaded_genes <- reactive({
      req(input$gene_file)
      
      file_path <- input$gene_file$datapath
      file_ext <- tools::file_ext(input$gene_file$name)
      
      tryCatch({
        if (file_ext %in% c("txt", "tsv")) {
          # Read as plain text file
          genes <- readLines(file_path)
          # Remove empty lines and trim whitespace
          genes <- trimws(genes[genes != ""])
        } else if (file_ext == "csv") {
          # Read as CSV and take first column
          data <- read.csv(file_path, header = FALSE, stringsAsFactors = FALSE)
          genes <- as.character(data[, 1])
          genes <- trimws(genes[!is.na(genes) & genes != ""])
        } else {
          return(NULL)
        }
        
        return(genes)
      }, error = function(e) {
        cat("Error reading file:", e$message, "\n")
        return(NULL)
      })
    })
    
    # Process manual input
    manual_genes <- reactive({
      req(input$manual_genes)
      
      if (input$manual_genes == "") return(NULL)
      
      # Split by new lines and commas, then clean up
      genes <- unlist(strsplit(input$manual_genes, "[\\n,]+"))
      genes <- trimws(genes[genes != ""])
      
      if (length(genes) == 0) return(NULL)
      
      return(genes)
    })
    
    # Combined gene list based on method
    processed_genes <- reactive({
      if (input$input_method == "manual") {
        return(manual_genes())
      } else {
        return(uploaded_genes())
      }
    })
    
    # Store final gene list when apply button is clicked
    final_genes <- reactive({
      input$apply_genes  # Dependency on button click
      
      isolate({
        genes <- processed_genes()
        if (is.null(genes) || length(genes) == 0) {
          return(NULL)
        }
        
        # Remove duplicates and sort
        genes <- unique(genes)
        genes <- sort(genes)
        
        cat("Applied", length(genes), "gene annotations\n")
        cat("Genes:", paste(head(genes, 10), collapse = ", "), 
            if (length(genes) > 10) "..." else "", "\n")
        
        return(genes)
      })
    })
    
    # Display current genes
    output$current_genes_display <- renderUI({
      genes <- final_genes()
      
      if (is.null(genes) || length(genes) == 0) {
        div(
          style = "color: #666; font-style: italic;",
          "No genes selected for annotation"
        )
      } else {
        div(
          tags$b("Current annotations (", length(genes), " genes):"),
          br(),
          div(
            style = "max-height: 150px; overflow-y: auto; border: 1px solid #ddd; padding: 10px; margin-top: 5px; background-color: #f8f9fa;",
            if (length(genes) <= 20) {
              paste(genes, collapse = ", ")
            } else {
              paste(c(genes[1:20], "..."), collapse = ", ")
            }
          )
        )
      }
    })
    
    # Return reactive values for external use
    return(list(
      selected_genes = final_genes
    ))
  })
}