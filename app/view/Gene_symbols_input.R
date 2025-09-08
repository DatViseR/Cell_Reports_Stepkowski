# This module provides a selectize input for users to enter custom gene symbols from genes present in
# the dataset. It supports multiple gene entries and provides autocomplete suggestions.
box::use(
  shiny[
    moduleServer,
    NS,
    selectizeInput,
    renderUI,
    uiOutput,
    reactive,
    req,
    div,
    p,
    tags,
    observe,
    updateSelectizeInput
  ],
  dplyr[filter, pull]
)

#' Create a gene symbols input UI
#'
#' @param id Module ID
#' @param label Input label (default: "Enter Gene Symbols")
#' @param placeholder Placeholder text (default: "Type gene symbols...")
#' @param width Input width (default: "100%")
#'
#' @return A UI element for gene symbol input
#' @export
ui <- function(
  id,
  label = "Enter Gene Symbols",
  placeholder = "Type gene symbols...",
  width = "100%"
) {
  ns <- NS(id)

  div(
    style = "margin-bottom: 20px;",
    p(tags$b(label)),
    uiOutput(ns("gene_input_ui"))
  )
}

#' Create a gene symbols input server
#'
#' @param id Module ID
#' @param dataset Static data frame containing a 'Gene_single' column (or other gene column)
#' @param gene_column Name of the gene column in the dataset (default: "Gene_single")
#' @return A reactive expression containing the selected gene symbols
#' @export
#'
#' @examples
#' # Example usage in main server
#' # datasets is the list from main server
#' gene_symbols <- Gene_symbols_input$server(
#'   "gene_input",
#'   dataset = datasets$I,
#'   gene_column = "Gene_single"
#' )
#' # In UI function
#' Gene_symbols_input$ui("gene_input")
#'
server <- function(id, dataset, gene_column = "Gene_single") {
  moduleServer(id, function(input, output, session) {
    # Extract unique gene symbols from the dataset
    gene_choices <- unique(dataset[[gene_column]])
    gene_choices <- gene_choices[!is.na(gene_choices)] # Remove any NA values

    output$gene_input_ui <- renderUI({
      selectizeInput(
        session$ns("gene_symbols"),
        label = NULL,
        choices = gene_choices,
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

    # Return selected gene symbols as a reactive expression
    selected_genes <- reactive({
      req(input$gene_symbols)
      input$gene_symbols
    })

    return(selected_genes)
  })
}
