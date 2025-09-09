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
    tags,
    observe,
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
  utils[head],
  dplyr[filter],
  app / view / GO_selection_module,
  app / view / GO_Color_picker,
  app / view / Gene_symbols_input,
  app / view / Gene_symbols_file_input,
  app / view / volcano_generic,
  app / view / GO_gene_mapper
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
        GO_selection_module$ui(ns("go_selection_silencing")),
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
        "EEF1A1 silencing ± CCCP proteome analysis",
        style = "color: #0062cc; margin-bottom: 20px;"
      ),
      p(
        "Explore Dataset III: EEF1A1 silencing with and without CCCP treatment using BONCAT LC-MS/MS proteomics.",
        style = "margin-bottom: 20px; color: #666;"
      ),
      card(
        card_header(
          h4(
            "Interactive volcano plots for all comparisons:",
            style = "margin: 0;"
          )
        ),
        card_body(
          br(),
          fluidRow(
            column(
              width = 6,
              div(
                tags$b("siEEF1A1 vs. Control"),
                style = "text-align: center; margin-top: 5px; color: #d62728;"
              ),
              volcano_generic$ui(ns("volcano_siEEF1A1_Control"))
            ),
            column(
              width = 6,
              div(
                tags$b("siEEF1A1+CCCP vs. Control+CCCP"),
                style = "text-align: center; margin-top: 5px; color: #2ca02c;"
              ),
              volcano_generic$ui(ns("volcano_siEEF1A1_CCCP_Control_CCCP"))
            )
          ),
          br(),
          fluidRow(
            column(
              width = 6,
              div(
                tags$b("Control+CCCP vs. Control"),
                style = "text-align: center; margin-top: 5px; color: #ff7f0e;"
              ),
              volcano_generic$ui(ns("volcano_Control_CCCP_Control"))
            ),
            column(
              width = 6,
              div(
                tags$b("siEEF1A1+CCCP vs. siEEF1A1"),
                style = "text-align: center; margin-top: 5px; color: #9467bd;"
              ),
              volcano_generic$ui(ns("volcano_siEEF1A1_CCCP_siEEF1A1"))
            )
          ),
          br(),
          div(
            style = "font-size: 90%; color: #666; text-align: center;",
            "EEF1A1 silencing experiments with and without CCCP treatment (8μM) - nascent proteome profiling with BONCAT LC-MS/MS"
          )
        )
      )
    )
  )
}

#' @export
server <- function(id, GO = NULL, datasets = NULL) {
  moduleServer(id, function(input, output, session) {
    if (is.null(datasets) || is.null(datasets$III)) {
      warning("Silencing_EEF1A1$server: 'datasets$III' not provided.")
    }

    # Initialize GO gene mapper
    go_mapper <- GO_gene_mapper$server("go_mapper", GO_data = GO)

    # GO selection + colors
    go_selection <- GO_selection_module$server(
      "go_selection_silencing",
      GO = GO
    )
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
      dataset = if (!is.null(datasets) && !is.null(datasets$III)) {
        unique(data.frame(Gene_single = datasets$III$`Gene names`))
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

    # Dataset III reactive (no transformation needed)
    dataset_iii <- reactive({
      if (is.null(datasets) || is.null(datasets$III)) {
        return(NULL)
      }

      df <- datasets$III
      cat("Dataset III structure - rows:", nrow(df), "\n")
      cat("Available columns:", paste(names(df), collapse = ", "), "\n")

      return(df)
    })

    # Volcano modules for the four comparisons using volcano_generic
    # Note: Column names are placeholders and may need adjustment based on actual Dataset 3 structure

    # 1. siEEF1A1 vs. Control
    volcano_generic$server(
      "volcano_Control_CCCP_Control",
      dataset = reactive({
        if (is.null(datasets) || is.null(datasets$III)) {
          return(NULL)
        }
        datasets$III |>
          filter(comparison == "siRNA EEF1A1(DMSO) vs. siRNA scramble (DMSO)")
      }),
      log2fc_column = "log2_fold",
      qvalue_column = "q_value",
      gene_column = "Gene names",
      go_annotations = go_highlights,
      custom_highlights = custom_genes,
      title = reactive("siRNA EEF1A1(DMSO) vs. siRNA scramble (DMSO)"),
      X_MIN = -2.9,
      X_MAX = 2.9,
      Y_MIN = 0,
      Y_MAX = 6.9
    )

    # 2. siEEF1A1+CCCP vs. Control+CCCP
    volcano_generic$server(
      "volcano_siEEF1A1_CCCP_Control_CCCP",
      dataset = reactive({
        if (is.null(datasets) || is.null(datasets$III)) {
          return(NULL)
        }
        datasets$III |>
          filter(comparison == "siRNA EEF1A1(CCCP) vs. siRNA scramble (CCCP)")
      }),
      log2fc_column = "Log2FC_siEEF1A1_CCCP_Control_CCCP", # Placeholder - may need adjustment
      qvalue_column = "q_value_siEEF1A1_CCCP_Control_CCCP", # Placeholder - may need adjustment
      gene_column = "Gene names",
      go_annotations = go_highlights,
      custom_highlights = custom_genes,
      title = reactive("siRNA EEF1A1(CCCP) vs. siRNA scramble (CCCP)"),
      X_MIN = -2,
      X_MAX = 2,
      Y_MIN = 0,
      Y_MAX = 6.9
    )

    # 3. Control+CCCP vs. Control
    volcano_generic$server(
      "volcano_Control_CCCP_Control",
      dataset = reactive({
        if (is.null(datasets) || is.null(datasets$III)) {
          return(NULL)
        }
        datasets$III |>
          filter(comparison == "siRNA EEF1A1(DMSO) vs. siRNA scramble (DMSO)")
      }),
      log2fc_column = "log2_fold",
      qvalue_column = "q_value",
      gene_column = "Gene names",
      go_annotations = go_highlights,
      custom_highlights = custom_genes,
      title = reactive("siRNA EEF1A1(DMSO) vs. siRNA scramble (DMSO)"),
      X_MIN = -2.9,
      X_MAX = 2.9,
      Y_MIN = 0,
      Y_MAX = 6.9
    )

    # 4. siEEF1A1+CCCP vs. siEEF1A1
    volcano_generic$server(
      "volcano_siEEF1A1_CCCP_siEEF1A1",
      dataset = reactive({
        if (is.null(datasets) || is.null(datasets$III)) {
          return(NULL)
        }
        datasets$III
      }),
      log2fc_column = "Log2FC_siEEF1A1_CCCP_siEEF1A1", # Placeholder - may need adjustment
      qvalue_column = "q_value_siEEF1A1_CCCP_siEEF1A1", # Placeholder - may need adjustment
      gene_column = "Gene names",
      go_annotations = go_highlights,
      custom_highlights = custom_genes,
      title = reactive("siEEF1A1+CCCP vs. siEEF1A1"),
      X_MIN = -2,
      X_MAX = 2,
      Y_MIN = 0,
      Y_MAX = 6.9
    )
  })
}
