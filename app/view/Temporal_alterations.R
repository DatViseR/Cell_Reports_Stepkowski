box::use(
  shiny[
    moduleServer,
    NS,
    selectInput,
    textAreaInput,
    actionButton,
    div,
    h3,
    h4,
    p,
    br,
    fluidRow,
    column,
    plotOutput,
    hr,
    conditionalPanel,
    updateSelectizeInput,
    tags,
    checkboxGroupInput,
    renderPlot,
    selectizeInput,
    observe,
    fileInput
  ],
  bslib[
    card,
    card_header,
    card_body,
    layout_sidebar,
    sidebar,
    accordion,
    accordion_panel,
    input_switch
  ],
  graphics[text],
  app / view / GO_selection_module,
  app / view / GO_Color_picker,
  app / view / Gene_symbols_input,
  app / view / Gene_symbols_file_input
)

#' @export
ui <- function(id, GO = NULL) {
  ns <- NS(id)

  layout_sidebar(
    sidebar = sidebar(
      width = 300,
      title = "Analysis Options",
      accordion(
        open = TRUE,
        GO_selection_module$ui(ns("go_selection_temporal")),
        GO_Color_picker$ui(ns("go_color_picker")),
        Gene_symbols_input$ui(ns("input_custom_genes")),
        Gene_symbols_file_input$ui(ns("input_custom_genes_file"))
      )
    ),
    div(
      h3(
        "Temporal Gene Expression Alterations",
        style = "color: #0062cc; margin-bottom: 20px;"
      ),
      card(
        card_header(
          h4("Differential Expression Across Time Points", style = "margin: 0;")
        ),
        card_body(
          p(
            "Volcano plots showing differential expression at each time point:"
          ),
          br(),
          fluidRow(
            column(
              width = 6,
              plotOutput(ns("volcano_2h"), height = "300px"),
              div(
                tags$b("2 hours"),
                style = "text-align: center; margin-top: 5px;"
              )
            ),
            column(
              width = 6,
              plotOutput(ns("volcano_6h"), height = "300px"),
              div(
                tags$b("6 hours"),
                style = "text-align: center; margin-top: 5px;"
              )
            )
          ),
          br(),
          fluidRow(
            column(
              width = 6,
              plotOutput(ns("volcano_12h"), height = "300px"),
              div(
                tags$b("12 hours"),
                style = "text-align: center; margin-top: 5px;"
              )
            ),
            column(
              width = 6,
              plotOutput(ns("volcano_24h"), height = "300px"),
              div(
                tags$b("24 hours"),
                style = "text-align: center; margin-top: 5px;"
              )
            )
          )
        )
      ),
      br(),
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
server <- function(id, GO = NULL, datasets = NULL) {
  moduleServer(id, function(input, output, session) {
    # Safety check
    if (is.null(datasets) || is.null(datasets$I)) {
      warning(
        "Temporal_alterations$server: 'datasets$I' not supplied; gene input modules will return empty vectors."
      )
    }

    # GO selection
    go_selection <- GO_selection_module$server("go_selection_temporal", GO = GO)

    # GO colors
    go_colors <- GO_Color_picker$server(
      "go_color_picker",
      chosen_go = go_selection$chosen_go
    )

    # Diagnostics
    observe({
      selected_go <- go_selection$chosen_go()
      if (length(selected_go)) {
        cat(
          "Selected GO categories:",
          paste(selected_go, collapse = ", "),
          "\n"
        )
      }
    })

    # Manual gene selection (from dataset I)
    manual_genes <- Gene_symbols_input$server(
      "input_custom_genes",
      dataset = if (!is.null(datasets)) {
        datasets$I
      } else {
        data.frame(Gene_single = character())
      },
      gene_column = "Gene_single"
    )

    # File-based gene upload
    file_genes <- Gene_symbols_file_input$server(
      "input_custom_genes_file",
      allowed_ext = c("txt", "tsv"),
      max_genes = 5000
    )

    # Combined gene set (union of both inputs)
    combined_genes <- shiny::reactive({
      unique(c(manual_genes(), file_genes()))
    })

    # Placeholder volcano plots
    output$volcano_2h <- renderPlot({
      plot(
        1:10,
        1:10,
        type = "n",
        xlab = "log2 Fold Change",
        ylab = "-log10(p-value)",
        main = "2h Volcano Plot (Placeholder)"
      )
      text(5, 5, "Volcano Plot\nPlaceholder", cex = 2)
    })

    output$volcano_6h <- renderPlot({
      plot(
        1:10,
        1:10,
        type = "n",
        xlab = "log2 Fold Change",
        ylab = "-log10(p-value)",
        main = "6h Volcano Plot (Placeholder)"
      )
      text(5, 5, "Volcano Plot\nPlaceholder", cex = 2)
    })

    output$volcano_12h <- renderPlot({
      plot(
        1:10,
        1:10,
        type = "n",
        xlab = "log2 Fold Change",
        ylab = "-log10(p-value)",
        main = "12h Volcano Plot (Placeholder)"
      )
      text(5, 5, "Volcano Plot\nPlaceholder", cex = 2)
    })

    output$volcano_24h <- renderPlot({
      plot(
        1:10,
        1:10,
        type = "n",
        xlab = "log2 Fold Change",
        ylab = "-log10(p-value)",
        main = "24h Volcano Plot (Placeholder)"
      )
      text(5, 5, "Volcano Plot\nPlaceholder", cex = 2)
    })

    # Placeholder heatmap
    output$temporal_heatmap <- renderPlot({
      plot(
        1:10,
        1:10,
        type = "n",
        xlab = "Time Points",
        ylab = "Genes",
        main = "Temporal Expression Heatmap (Placeholder)"
      )
      text(5, 5, "Heatmap\nPlaceholder", cex = 3)
    })

    # (Optional) expose combined genes for parent usage
    # return(list(
    #   genes = combined_genes,
    #   go = go_selection$chosen_go,
    #   colors = go_colors$chosen_colors
    # ))
  })
}
