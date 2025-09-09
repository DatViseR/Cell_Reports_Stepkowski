box::use(
  shiny[
    moduleServer,
    NS,
    div,
    h3,
    h4,
    p,
    br,

    fluidRow,
    column,
    plotOutput,
    tags,
    observe,
    renderPlot,
    reactive,
    actionButton,
    observeEvent,
    eventReactive,
    isolate,
    updateActionButton
  ],
  bslib[
    card,
    card_header,
    card_body,
    layout_sidebar,
    sidebar,
    accordion
  ],
  graphics[text],
  utils[head],
  app / view / GO_selection_module,
  app / view / GO_Color_picker,
  app / view / Gene_symbols_input,
  app / view / Gene_symbols_file_input,
  app / view / volcano,
  app / view / GO_gene_mapper # Add this import
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
      ),
      br(),
      div(
        style = "margin-top: 20px; text-align: center;",
        shiny::actionButton(
          ns("draw_plots"),
          "Draw volcano plots",
          class = "btn-primary",
          style = "width: 80%; font-weight: bold;"
        )
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
          p("Interactive volcano plots for each time point / condition:"),
          br(),
          fluidRow(
            column(
              width = 6,
              volcano$ui(ns("volcano_STRESS_I"), height = "300px"),
              div(
                tags$b("STRESS_I"),
                style = "text-align: center; margin-top: 5px;"
              )
            ),
            column(
              width = 6,
              volcano$ui(ns("volcano_STRESS_II"), height = "300px"),
              div(
                tags$b("STRESS_II"),
                style = "text-align: center; margin-top: 5px;"
              )
            )
          ),
          br(),
          fluidRow(
            column(
              width = 6,
              volcano$ui(ns("volcano_RECOVERY_I"), height = "300px"),
              div(
                tags$b("RECOVERY_I"),
                style = "text-align: center; margin-top: 5px;"
              )
            ),
            column(
              width = 6,
              volcano$ui(ns("volcano_RECOVERY_II"), height = "300px"),
              div(
                tags$b("RECOVERY_II"),
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
            "Rows represent genes; columns represent STRESS_I, STRESS_II, RECOVERY_I, RECOVERY_II."
          )
        )
      )
    )
  )
}

#' @export
server <- function(id, GO = NULL, datasets = NULL) {
  moduleServer(id, function(input, output, session) {
    if (is.null(datasets) || is.null(datasets$I)) {
      warning("Temporal_alterations$server: 'datasets$I' not provided.")
    }

    # Initialize GO gene mapper
    go_mapper <- GO_gene_mapper$server("go_mapper", GO_data = GO)

    # GO selection + colors
    go_selection <- GO_selection_module$server("go_selection_temporal", GO = GO)
    go_colors <- GO_Color_picker$server(
      "go_color_picker",
      chosen_go = go_selection$chosen_go
    )

    observe({
      sel <- go_selection$chosen_go()
      if (length(sel)) {
        cat("Selected GO categories:", paste(sel, collapse = ", "), "\n")
      }
    })

    # User custom genes (manual + file)
    manual_genes <- Gene_symbols_input$server(
      "input_custom_genes",
      dataset = if (!is.null(datasets) && !is.null(datasets$I)) {
        unique(data.frame(Gene_single = datasets$I$Gene_single))
      } else {
        data.frame(Gene_single = character())
      },
      gene_column = "Gene_single"
    )

    file_genes <- Gene_symbols_file_input$server(
      "input_custom_genes_file",
      allowed_ext = c("txt", "tsv"),
      max_genes = 5000
    )

    custom_genes <- eventReactive(
      input$draw_plots,
      {
        manual <- isolate(manual_genes())
        file <- isolate(file_genes())
        result <- unique(c(manual, file))
        cat(
          "Action button clicked - processing custom genes: ",
          length(result),
          "genes\n"
        )
        if (length(result) > 0) {
          cat(
            "Sample custom genes:",
            paste(head(result, 5), collapse = ", "),
            "\n"
          )
        }
        result
      },
      ignoreNULL = FALSE
    )

    # Update button text when button is clicked
    observeEvent(input$draw_plots, {
      updateActionButton(
        session,
        "draw_plots",
        label = "Redraw volcano plots"
      )
    })

    # Build GO highlight list: list of lists(category, genes, color)
    # Use eventReactive to only update when button is clicked
    go_highlights <- eventReactive(
      input$draw_plots,
      {
        cat("Action button clicked - processing GO highlights\n")
        sel <- isolate(go_selection$chosen_go())
        if (!length(sel)) {
          cat("No GO categories selected\n")
          return(NULL)
        }

        cols <- isolate(go_colors$go_colors())
        cat("Selected GO categories:", paste(sel, collapse = ", "), "\n")
        cat(
          "Color assignments:",
          if (is.null(cols)) "NULL" else length(cols),
          "\n"
        )

        out <- lapply(sel, function(go_cat) {
          # Use the GO mapper to get genes for this category
          genes <- unique(go_mapper$get_genes_for_go(go_cat))
          if (!length(genes)) {
            cat("No genes found for GO category:", go_cat, "\n")
            return(NULL)
          }
          cat("Found", length(genes), "genes for GO category:", go_cat, "\n")

          # Debug: print first few genes
          cat("Sample genes:", paste(head(genes, 5), collapse = ", "), "\n")

          list(
            category = go_cat,
            genes = genes,
            color = if (!is.null(cols) && go_cat %in% names(cols)) {
              cols[[go_cat]]
            } else {
              "#FF9900"
            }
          )
        })
        # Remove NULLs
        out <- Filter(Negate(is.null), out)
        if (!length(out)) {
          cat("No valid GO highlights created\n")
          return(NULL)
        }
        cat("Created", length(out), "GO highlight groups\n")
        out
      },
      ignoreNULL = FALSE
    ) # Allow initial execution even with NULL input

    # Volcano modules with timepoint-based filtering
    volcano$server(
      "volcano_STRESS_I",
      dataset = reactive({
        if (is.null(datasets) || is.null(datasets$I)) {
          return(NULL)
        }
        datasets$I
      }),
      timepoint = "STRESS_I",
      go_annotations = go_highlights,
      custom_highlights = custom_genes,
      title = reactive("STRESS_I Volcano")
    )

    volcano$server(
      "volcano_STRESS_II",
      dataset = reactive({
        if (is.null(datasets) || is.null(datasets$I)) {
          return(NULL)
        }
        datasets$I
      }),
      timepoint = "STRESS_II",
      go_annotations = go_highlights,
      custom_highlights = custom_genes,
      title = reactive("STRESS_II Volcano")
    )

    volcano$server(
      "volcano_RECOVERY_I",
      dataset = reactive({
        if (is.null(datasets) || is.null(datasets$I)) {
          return(NULL)
        }
        datasets$I
      }),
      timepoint = "RECOVERY_I",
      go_annotations = go_highlights,
      custom_highlights = custom_genes,
      title = reactive("RECOVERY_I Volcano")
    )

    volcano$server(
      "volcano_RECOVERY_II",
      dataset = reactive({
        if (is.null(datasets) || is.null(datasets$I)) {
          return(NULL)
        }
        datasets$I
      }),
      timepoint = "RECOVERY_II",
      go_annotations = go_highlights,
      custom_highlights = custom_genes,
      title = reactive("RECOVERY_II Volcano")
    )

    # Heatmap placeholder (unchanged)
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
  })
}
