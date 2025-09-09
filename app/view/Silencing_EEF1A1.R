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
      if (
        !all(
          c("comparison", "log2_fold", "q_value", "Gene names") %in% names(df)
        )
      ) {
        warning(
          "Dataset III is missing required columns: comparison, log2_fold, q_value, Gene names"
        )
      }
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
      dataset = unique(data.frame(Gene_single = datasets$III$`Gene names`)),
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

      # X limits (symmetric)
      x_abs <- max(abs(df$log2_fold), na.rm = TRUE)
      if (!is.finite(x_abs)) {
        x_abs <- 2
      }
      # Round up to a single decimal for nicer scale
      x_abs <- ceiling(x_abs * 10) / 10

      # Y limit: -log10(q_value)
      y_vals <- -log10(df$q_value)
      y_max <- max(y_vals[is.finite(y_vals)], na.rm = TRUE)
      if (!is.finite(y_max)) {
        y_max <- 5
      }
      # Add a small headroom
      y_max <- ceiling(y_max * 10) / 10 + 0.2

      list(xmin = -x_abs, xmax = x_abs, ymin = 0, ymax = y_max)
    })

    # print comparison labels for debugging in the console
    observe({
      df <- dataset_iii()
      if (is.null(df)) {
        return()
      }
      cat("Available comparisons in Dataset III:\n")
      print(unique(df$comparison))
    })

    # ---- Expected comparisons (adjust these if actual dataset uses different labels) ----
    expected_comparisons <- c(
      "siRNA EEF1A1(DMSO) vs. siRNA scramble (DMSO)",
      "siRNA EEF1A1(CCCP) vs. siRNA scramble (CCCP)",
      "siRNA scramble(CCCP) vs. siRNA scramble (DMSO)",
      "siRNA EEF1A1(CCCP) vs. siRNA EEF1A1(DMSO)"
    )

    present <- intersect(expected_comparisons, unique(datasets$III$comparison))
    if (length(present) < length(expected_comparisons)) {
      missing <- setdiff(expected_comparisons, present)
      warning(
        "Missing expected comparisons in dataset III: ",
        paste(missing, collapse = "; ")
      )
    }

    # Map UI IDs to comparison strings (order corresponds to UI layout)
    comparison_map <- list(
      volcano_siEEF1A1_Control = "siRNA EEF1A1(DMSO) vs. siRNA scramble (DMSO)",
      volcano_siEEF1A1_CCCP_Control_CCCP = "siRNA EEF1A1(CCCP) vs. siRNA scramble (CCCP)",
      volcano_Control_CCCP_Control = "Control+CCCP vs. Control",
      volcano_siEEF1A1_CCCP_siEEF1A1 = "siRNA EEF1A1(CCCP) vs. siRNA EEF1A1(DMSO)"
    )

    # ---- Instantiate volcano modules dynamically ----
    lapply(names(comparison_map), function(id) {
      comp_label <- comparison_map[[id]]

      volcano_generic$server(
        id = id,
        dataset = reactive({
          df <- dataset_iii()
          if (is.null(df)) {
            return(NULL)
          }
          df |> filter(.data$comparison == comp_label)
        }),
        log2fc_column = "log2_fold",
        qvalue_column = "q_value",
        gene_column = "Gene names",
        go_annotations = go_highlights,
        custom_highlights = custom_genes,
        title = reactive(comp_label),
        X_MIN = axis_limits()$xmin,
        X_MAX = axis_limits()$xmax,
        Y_MIN = axis_limits()$ymin,
        Y_MAX = axis_limits()$ymax
      )
    })
  })
}
