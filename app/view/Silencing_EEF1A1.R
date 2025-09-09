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
                tags$b("siRNA EEF1A1(DMSO) vs. siRNA scramble (DMSO)"),
                style = "text-align: center; margin-top: 5px; color: #d62728;"
              ),
              volcano_generic$ui(ns("volcano_siEEF1A1_Control"))
            ),
            column(
              width = 6,
              div(
                tags$b("siRNA EEF1A1(CCCP) vs. siRNA scramble (CCCP)"),
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
                tags$b("siRNA EEF1A1(CCCP) vs. siRNA EEF1A1(DMSO)"),
                style = "text-align: center; margin-top: 5px; color: #9467bd;"
              ),
              volcano_generic$ui(ns("volcano_siEEF1A1_CCCP_siEEF1A1"))
            )
          ),
          br(),
          div(
            style = "font-size: 90%; color: #666; text-align: center;",
            "EEF1A1 silencing experiments with and without CCCP (8 μM) - nascent proteome profiling (BONCAT LC-MS/MS)"
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
      return(invisible(NULL))
    }

    # ---- Helper: dataset reactive ----
    dataset_iii <- reactive({
      df <- datasets$III
      if (is.null(df)) {
        return(NULL)
      }
      
      # Dataset 3 has separate Log2FC columns for each comparison, not a single comparison column
      expected_cols <- c("Gene names", 
                        "Log2FC A1_Veh_SC_Veh", "T-test q-value A1_Veh_SC_Veh",
                        "Log2FC A1_CCCP_SC_CCCP", "T-test q-value A1_CCCP_SC_CCCP", 
                        "Log2FC SC_CCCP_SC_Veh", "T-test q-value SC_CCCP_SC_Veh",
                        "Log2FC A1_CCCP_A1_Veh", "T-test q-value A1_CCCP_A1_Veh")
      
      missing <- setdiff(expected_cols, names(df))
      if (length(missing) > 0) {
        warning("Dataset III is missing expected columns: ", paste(missing, collapse = ", "))
      }
      
      cat("Dataset III structure - rows:", nrow(df), "\n")
      cat("Available columns:", paste(names(df), collapse = ", "), "\n")
      
      df
    })

    # ---- GO Mapper and selection/color modules ----
    go_mapper <- GO_gene_mapper$server("go_mapper", GO_data = GO)

    go_selection <- GO_selection_module$server(
      "go_selection_silencing",
      GO = GO
    )

    go_colors <- GO_Color_picker$server(
      "go_color_picker",
      chosen_go = go_selection$chosen_go
    )

    # ---- Custom gene inputs ----
    manual_genes <- Gene_symbols_input$server(
      "input_custom_genes",
      dataset = reactive({
        df <- dataset_iii()
        if (is.null(df)) {
          return(data.frame(Gene_single = character()))
        }
        unique(data.frame(Gene_single = df$`Gene names`))
      }),
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
        unique(c(manual, file))
      },
      ignoreNULL = FALSE
    )

    observeEvent(input$draw_plots, {
      updateActionButton(session, "draw_plots", label = "Redraw volcano plots")
    })

    # ---- GO highlight groups (on demand) ----
    go_highlights <- eventReactive(
      input$draw_plots,
      {
        sel <- isolate(go_selection$chosen_go())
        if (!length(sel)) {
          return(NULL)
        }
        cols <- isolate(go_colors$go_colors())

        out <- lapply(sel, function(go_cat) {
          genes <- unique(go_mapper$get_genes_for_go(go_cat))
          if (!length(genes)) {
            return(NULL)
          }
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
        Filter(Negate(is.null), out)
      },
      ignoreNULL = FALSE
    )

    # ---- Derive unified axis limits across ALL comparisons ----
    axis_limits <- reactive({
      df <- dataset_iii()
      if (is.null(df)) {
        return(list(xmin = -2, xmax = 2, ymin = 0, ymax = 5))
      }

      # Get all log2FC values from all four comparisons  
      all_log2fc <- c(
        df$`Log2FC A1_Veh_SC_Veh`,
        df$`Log2FC A1_CCCP_SC_CCCP`,
        df$`Log2FC SC_CCCP_SC_Veh`, 
        df$`Log2FC A1_CCCP_A1_Veh`
      )
      all_log2fc <- all_log2fc[is.finite(all_log2fc)]

      # X limits (symmetric around 0, using max absolute value)
      x_abs <- max(abs(all_log2fc), na.rm = TRUE)
      if (!is.finite(x_abs) || x_abs == 0) {
        x_abs <- 2
      }
      # Round up to single decimal for nicer scale
      x_abs <- ceiling(x_abs * 10) / 10

      # Get all q-values from all four comparisons
      all_qvalues <- c(
        df$`T-test q-value A1_Veh_SC_Veh`,
        df$`T-test q-value A1_CCCP_SC_CCCP`,
        df$`T-test q-value SC_CCCP_SC_Veh`,
        df$`T-test q-value A1_CCCP_A1_Veh`
      )
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

      cat("Computed unified axis limits - X: [-", x_abs, ", ", x_abs, "], Y: [0, ", y_max, "]\n")

      list(xmin = -x_abs, xmax = x_abs, ymin = 0, ymax = y_max)
    })

    # print some debug info about the data
    observe({
      df <- dataset_iii()
      if (is.null(df)) {
        return()
      }
      cat("Dataset III column names:\n")
      print(names(df))
      
      # Show sample Log2FC values for each comparison
      comparisons <- list(
        "A1_Veh_SC_Veh" = "Log2FC A1_Veh_SC_Veh",
        "A1_CCCP_SC_CCCP" = "Log2FC A1_CCCP_SC_CCCP", 
        "SC_CCCP_SC_Veh" = "Log2FC SC_CCCP_SC_Veh",
        "A1_CCCP_A1_Veh" = "Log2FC A1_CCCP_A1_Veh"
      )
      
      for (comp_name in names(comparisons)) {
        col_name <- comparisons[[comp_name]]
        if (col_name %in% names(df)) {
          vals <- df[[col_name]][is.finite(df[[col_name]])]
          if (length(vals) > 0) {
            cat(comp_name, "range:", range(vals, na.rm = TRUE), "\n")
          } else {
            cat(comp_name, ": no finite values\n")
          }
        } else {
          cat(comp_name, ": column not found\n")
        }
      }
    })

    # ---- Define the four comparisons and their corresponding volcano modules ----
    # Map UI IDs to column names (Dataset 3 has separate columns for each comparison)
    comparison_configs <- list(
      volcano_siEEF1A1_Control = list(
        log2fc_col = "Log2FC A1_Veh_SC_Veh",
        qvalue_col = "T-test q-value A1_Veh_SC_Veh", 
        title = "siRNA EEF1A1(DMSO) vs. siRNA scramble (DMSO)"
      ),
      volcano_siEEF1A1_CCCP_Control_CCCP = list(
        log2fc_col = "Log2FC A1_CCCP_SC_CCCP",
        qvalue_col = "T-test q-value A1_CCCP_SC_CCCP",
        title = "siRNA EEF1A1(CCCP) vs. siRNA scramble (CCCP)" 
      ),
      volcano_Control_CCCP_Control = list(
        log2fc_col = "Log2FC SC_CCCP_SC_Veh",
        qvalue_col = "T-test q-value SC_CCCP_SC_Veh",
        title = "Control+CCCP vs. Control"
      ),
      volcano_siEEF1A1_CCCP_siEEF1A1 = list(
        log2fc_col = "Log2FC A1_CCCP_A1_Veh", 
        qvalue_col = "T-test q-value A1_CCCP_A1_Veh",
        title = "siRNA EEF1A1(CCCP) vs. siRNA EEF1A1(DMSO)"
      )
    )

    # ---- Instantiate volcano modules dynamically ----
    lapply(names(comparison_configs), function(id) {
      config <- comparison_configs[[id]]

      volcano_generic$server(
        id = id,
        dataset = dataset_iii, # Use the full dataset, not filtered
        log2fc_column = config$log2fc_col,
        qvalue_column = config$qvalue_col,
        gene_column = "Gene names",
        go_annotations = go_highlights,
        custom_highlights = custom_genes,
        title = reactive(config$title),
        X_MIN = reactive(axis_limits()$xmin),
        X_MAX = reactive(axis_limits()$xmax),
        Y_MIN = reactive(axis_limits()$ymin),
        Y_MAX = reactive(axis_limits()$ymax)
      )
    })
  })
}
