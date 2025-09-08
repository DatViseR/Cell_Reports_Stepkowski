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
        # enable server site filtering for large datasets

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
# This module provides a performant (server-side) selectize input for custom gene symbols.
# It supports large datasets by deferring searching/filtering to the server.
#
# Public API:
#   Gene_symbols_input$ui(id,
#       label = "Enter Gene Symbols",
#       placeholder = "Type gene symbols...",
#       width = "100%")
#
#   Gene_symbols_input$server(id,
#       dataset,
#       gene_column = "Gene_single",
#       max_items = 100,
#       max_options = 500,
#       allow_create = TRUE)
#
# Returns:
#   reactive character vector of selected (and possibly user-created) gene symbols.
#
box::use(
  shiny[
    moduleServer,
    NS,
    selectizeInput,
    renderUI,
    uiOutput,
    reactive,
    updateSelectizeInput,
    observe,
    observeEvent,
    isolate,
    div,
    p,
    tags
  ],
  dplyr[distinct, pull]
)

# ---------------------------
# UI
# ---------------------------
#' Gene symbols input UI
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
    # We render a selectizeInput with choices = NULL for fast initial load.
    selectizeInput(
      inputId = ns("gene_symbols"),
      label = NULL,
      choices = NULL, # IMPORTANT: start empty; server will populate
      multiple = TRUE,
      width = width,
      options = list(
        placeholder = placeholder
      )
    )
  )
}

# ---------------------------
# SERVER
# ---------------------------
#' Gene symbols input server
#'
#' @param id Module id
#' @param dataset A (static) data.frame containing the gene column
#' @param gene_column Column name containing gene symbols
#' @param max_items Maximum number of selections user can make (default 100)
#' @param max_options Maximum number of options to display in dropdown at once (default 500)
#' @param allow_create Whether to allow user to type and add genes not present in dataset (default TRUE)
#'
#' @return reactive character vector of selected gene symbols (may be empty)
#' @export
server <- function(
  id,
  dataset,
  gene_column = "Gene_single",
  max_items = 100,
  max_options = 500,
  allow_create = TRUE
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Preprocess gene list once (static dataset assumed)
    gene_choices <- reactive({
      if (is.null(dataset) || !nrow(dataset)) {
        return(character())
      }
      if (!gene_column %in% names(dataset)) {
        warning(
          "Gene_symbols_input: gene_column '",
          gene_column,
          "' not found in dataset."
        )
        return(character())
      }
      genes <- dataset[[gene_column]]
      genes <- genes[!is.na(genes)]
      genes <- trimws(genes)
      genes <- genes[nzchar(genes)]
      unique(genes)
    })

    # Initialize / update selectize with server-side mode
    observeEvent(
      gene_choices(),
      {
        updateSelectizeInput(
          session,
          "gene_symbols",
          choices = gene_choices(),
          server = TRUE, # crucial for large option sets
          options = list(
            # limit how many matches are shown per query
            maxOptions = max_options,
            # allow user-created (novel) entries
            create = allow_create,
            # limit number of selections
            maxItems = max_items,
            # search both label and value (default)
            searchField = c("label", "value"),
            # optional: diacritics can be skipped; leave default if not needed
            # diacritics = TRUE
            placeholder = NULL
          )
        )
      },
      ignoreNULL = FALSE
    )

    # Reactive selected genes (allow empty vector)
    selected_genes <- reactive({
      val <- input$gene_symbols
      if (is.null(val)) {
        return(character())
      }
      unique(val)
    })

    return(selected_genes)
  })
}
