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
        GO_selection_module$ui(ns("go_selection_cccp_bortezomib")),
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
        "CCCP vs. Bortezomib proteome analysis",
        style = "color: #0062cc; margin-bottom: 20px;"
      ),
      p(
        "Explore Dataset II: CCCP or Bortezomib vs. DMSO using BONCAT LC-MS/MS with 1 hour AHA pulse labeling and TMT multiplexed proteomics.",
        style = "margin-bottom: 20px; color: #666;"
      ),
      card(
        card_header(
          h4(
            "Interactive volcano plots for treatment comparisons:",
            style = "margin: 0;"
          )
        ),
        card_body(
          br(),
          fluidRow(
            column(
              width = 6,
              div(
                tags$b("CCCP vs. DMSO"),
                style = "text-align: center; margin-top: 5px; color: #d62728;"
              ),
              volcano_generic$ui(ns("volcano_CCCP_DMSO"))
            ),
            column(
              width = 6,
              div(
                tags$b("Bortezomib vs. DMSO"),
                style = "text-align: center; margin-top: 5px; color: #2ca02c;"
              ),
              volcano_generic$ui(ns("volcano_Bortezomib_DMSO"))
            )
          ),
          br(),
          div(
            style = "font-size: 90%; color: #666; text-align: center;",
            "Treatment conditions: 2.5h pretreatment with 20nM Bortezomib/DMSO followed by 1h labeling with 2mM AHA together with 20nM Bortezomib or 8μM CCCP or DMSO (n=4 replicates)"
          )
        )
      )
    )
  )
}

#' @export
server <- function(id, GO = NULL, datasets = NULL) {
  moduleServer(id, function(input, output, session) {
    if (is.null(datasets) || is.null(datasets$II)) {
      warning("CCCP_vs_Bortezomib$server: 'datasets$II' not provided.")
    }

    # Initialize GO gene mapper
    go_mapper <- GO_gene_mapper$server("go_mapper", GO_data = GO)

    # GO selection + colors
    go_selection <- GO_selection_module$server(
      "go_selection_cccp_bortezomib",
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
      dataset = if (!is.null(datasets) && !is.null(datasets$II)) {
        unique(data.frame(Gene_single = datasets$II$Gene_names))
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

    # Dataset II reactive (no transformation needed)  
    dataset_ii <- reactive({
      if (is.null(datasets) || is.null(datasets$II)) {
        return(NULL)
      }

      df <- datasets$II
      cat("Dataset II structure - rows:", nrow(df), "\n")
      cat("Available columns:", paste(names(df), collapse = ", "), "\n")

      return(df)
    })

    # Compute dynamic axis limits based on data
    axis_limits <- reactive({
      df <- dataset_ii()
      if (is.null(df)) {
        return(list(xmin = -2, xmax = 2, ymin = 0, ymax = 5))
      }
      
      # Get all log2FC values from both comparisons
      all_log2fc <- c(df$Log2FC_CCCP_DMSO, df$Log2FC_Bortezomib_DMSO)
      all_log2fc <- all_log2fc[is.finite(all_log2fc)]
      
      # X limits (symmetric around 0, using max absolute value)
      x_abs <- max(abs(all_log2fc), na.rm = TRUE)
      if (!is.finite(x_abs) || x_abs == 0) {
        x_abs <- 2
      }
      # Round up to single decimal for nicer scale
      x_abs <- ceiling(x_abs * 10) / 10
      
      # Get all q-values from both comparisons  
      all_qvalues <- c(df$`q-value_CCCP_DMSO`, df$`q-value_Bortezomib_DMSO`)
      all_qvalues <- all_qvalues[is.finite(all_qvalues) & all_qvalues > 0]
      
      # Y limit: -log10(q_value), use minimum q-value for max y
      if (length(all_qvalues) > 0) {
        min_qvalue <- min(all_qvalues, na.rm = TRUE)
        y_max <- -log10(min_qvalue)
        if (!is.finite(y_max) || y_max <= 0) {
          y_max <- 5
        }
        # Add headroom
        y_max <- ceiling(y_max * 10) / 10 + 0.5
      } else {
        y_max <- 5
      }
      
      cat("Computed axis limits - X: [-", x_abs, ", ", x_abs, "], Y: [0, ", y_max, "]\n")
      
      list(xmin = -x_abs, xmax = x_abs, ymin = 0, ymax = y_max)
    })

    # Volcano modules for the two comparisons using volcano_generic
    volcano_generic$server(
      "volcano_CCCP_DMSO",
      dataset = reactive({
        if (is.null(datasets) || is.null(datasets$II)) {
          return(NULL)
        }
        datasets$II
      }),
      log2fc_column = "Log2FC_CCCP_DMSO",
      qvalue_column = "q-value_CCCP_DMSO",
      gene_column = "Gene_names",
      go_annotations = go_highlights,
      custom_highlights = custom_genes,
      title = reactive("CCCP vs. DMSO"),
      X_MIN = reactive(axis_limits()$xmin),
      X_MAX = reactive(axis_limits()$xmax),
      Y_MIN = reactive(axis_limits()$ymin),
      Y_MAX = reactive(axis_limits()$ymax)
    )

    volcano_generic$server(
      "volcano_Bortezomib_DMSO",
      dataset = reactive({
        if (is.null(datasets) || is.null(datasets$II)) {
          return(NULL)
        }
        datasets$II
      }),
      log2fc_column = "Log2FC_Bortezomib_DMSO",
      qvalue_column = "q-value_Bortezomib_DMSO",
      gene_column = "Gene_names",
      go_annotations = go_highlights,
      custom_highlights = custom_genes,
      title = reactive("Bortezomib vs. DMSO"),
      X_MIN = reactive(axis_limits()$xmin),
      X_MAX = reactive(axis_limits()$xmax),
      Y_MIN = reactive(axis_limits()$ymin),
      Y_MAX = reactive(axis_limits()$ymax)
    )
  })
}
