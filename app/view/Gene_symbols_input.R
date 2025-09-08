# This module provides a selectize input for users to enter custom gene symbols from genes present in 
# the dataset. It supports multiple gene entries and provides autocomplete suggestions.
box::use(
  shiny[moduleServer, NS, selectizeInput, renderUI, uiOutput, reactive, req, div, p, tags],
  dplyr[filter]
)
#' Create a gene symbols input UI
#' 
#' @param id Module ID
#' @param label Input label (default: "Enter Gene Symbols")
#' @param placeholder Placeholder text (default: "Type gene symbols...")
#' @param width Input width (default: "100%")
#' @param dataset Reactive expression containing a data frame with a 'genes' column
#' 
#' @return A UI element for gene symbol input
#'
#'  @export

ui <- function(id, label = "Enter Gene Symbols", placeholder = "Type gene symbols...", width = "100%", dataset) {
  ns <- NS(id)
  
  div(
    style = "margin-bottom: 20px;",
    p(tags$b(label)),
    uiOutput(ns("gene_input_ui"))
  )
}

#' 
#' Create a gene symbols input server
#' 
#' @param id Module ID
#' @param dataset Reactive expression containing a data frame with a 'genes' column
#' @return A reactive expression containing the selected gene symbols
#' @export
#' 
#' @examples
#' #' # Example dataset
#' dataset <- reactive({
#'  data.frame(genes = c("GeneA", "GeneB", "GeneC", "GeneD"))
#' })
#' #' # In server function
#' gene_symbols <- Gene_symbols_input$server("gene_input", dataset)
#' #' # In UI function
#' Gene_symbols_input$ui("gene_input", dataset = dataset)
#' 
#' 
#' server <- function(id, dataset) {
  moduleServer(id, function(input, output, session) {
    output$gene_input_ui <- renderUI({
      req(dataset())
      selectizeInput(
        session$ns("gene_symbols"),
        label = NULL,
        choices = NULL,
        selected = NULL,
        multiple = TRUE,
        options = list(
          placeholder = "Type gene symbols...",
          create = TRUE,
          maxItems = 100
        ),
        width = "100%"
      )
    })
    
    # Update choices based on dataset
    observe({
      req(dataset())
      updateSelectizeInput(
        session,
        "gene_symbols",
        choices = unique(dataset()$genes),
        server = TRUE
      )
    })
    
    # Return selected gene symbols as a reactive expression
    selected_genes <- reactive({
      req(input$gene_symbols)
      input$gene_symbols
    })
    
    return(selected_genes)
  })
}