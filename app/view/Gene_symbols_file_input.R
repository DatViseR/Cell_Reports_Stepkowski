# Module: Gene_symbols_file_input
# Allows users to upload a plain text file containing gene symbols separated
# by commas, spaces, semicolons, tabs, or new lines. Returns a reactive vector
# of cleaned, unique gene symbols.
#
# Expected file formats:
#   GeneA,GeneB,GeneC
#   GeneA
#   GeneB
#   GeneC
#   GeneA;GeneB;GeneC
#
# Empty lines and duplicates are removed. Case is preserved.
#
# Dependencies: shiny, stringr, tools (optional)
box::use(
  shiny[
    moduleServer,
    NS,
    fileInput,
    p,
    tags,
    div,
    renderUI,
    uiOutput,
    reactive,
    req,
    validate,
    need,
    downloadButton,
    downloadHandler,
    checkboxInput,
    verbatimTextOutput,
    renderPrint
  ],
  stringr[str_split, str_trim, str_squish],
  utils[read.table]
)

#' Gene Symbols File Input UI
#'
#' @param id Module id
#' @param label Label shown above upload control
#' @param width Width CSS (default 100%)
#'
#' @export
ui <- function(id, label = "Upload Gene Symbols (TXT)", width = "100%") {
  ns <- NS(id)

  div(
    style = sprintf("width:%s; margin-bottom: 15px;", width),
    p(tags$b(label)),
    uiOutput(ns("file_input_ui")),
    div(
      style = "margin-top:8px;",
      checkboxInput(ns("show_preview"), "Show parsed genes", value = FALSE)
    ),
    uiOutput(ns("preview_container"))
  )
}

#' Gene Symbols File Input Server
#'
#' @param id Module id
#' @param allowed_ext Character vector of allowed file extensions (default c(\"txt\", \"tsv\"))
#' @param max_genes Optional cap on number of genes parsed (NULL = no cap)
#'
#' @return A reactive expression returning a character vector of unique gene symbols,
#'         or character(0) if no valid file has been uploaded.
#' @export
server <- function(id, allowed_ext = c("txt", "tsv"), max_genes = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$file_input_ui <- renderUI({
      fileInput(
        ns("genes_file"),
        label = NULL,
        accept = c(".txt", ".tsv", "text/plain"),
        buttonLabel = "Browse",
        placeholder = "No file selected"
      )
    })

    # Reactive that parses uploaded file
    parsed_genes <- reactive({
      file <- input$genes_file
      if (is.null(file)) {
        return(character(0))
      }

      ext <- tolower(tools::file_ext(file$name))
      validate(
        need(
          ext %in% allowed_ext,
          sprintf(
            "Invalid file extension '.%s'. Allowed: %s",
            ext,
            paste(allowed_ext, collapse = ", ")
          )
        )
      )

      # Read entire file as one string
      raw_text <- tryCatch(
        paste(readLines(file$datapath, warn = FALSE), collapse = "\n"),
        error = function(e) ""
      )

      if (nchar(raw_text) == 0) {
        return(character(0))
      }

      # Normalize separators: replace semicolons, tabs, carriage returns with newlines
      norm <- gsub("[;,\\t\\r]", "\n", raw_text)
      # Also allow spaces after commas etc already handled; now split on newlines
      parts <- unlist(str_split(norm, "\n"))
      parts <- str_trim(parts)
      parts <- parts[nzchar(parts)] # remove empty
      # Some users might put multiple genes on one line separated by spaces
      # If a 'part' still contains spaces, split further
      further <- unlist(str_split(parts, "[\\s,]+"))
      further <- str_trim(further)
      further <- further[nzchar(further)]

      genes <- unique(further)

      if (!is.null(max_genes) && length(genes) > max_genes) {
        genes <- genes[seq_len(max_genes)]
      }

      genes
    })

    # Preview UI
    output$preview_container <- renderUI({
      req(input$show_preview)
      genes <- parsed_genes()
      if (length(genes) == 0) {
        return(
          div(
            style = "font-size: 90%; color:#888; font-style: italic;",
            "No valid genes parsed yet."
          )
        )
      }

      div(
        style = "border:1px solid #ddd; padding:6px; border-radius:4px; background:#fafafa; max-height:200px; overflow:auto;",
        tags$code(paste(genes, collapse = ", "))
      )
    })

    return(parsed_genes)
  })
}
