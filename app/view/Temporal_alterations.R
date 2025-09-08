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
    reactive
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
  app / view / GO_selection_module,
  app / view / GO_Color_picker,
  app / view / Gene_symbols_input,
  app / view / Gene_symbols_file_input,
  app / view / volcano
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

    custom_genes <- reactive({
      unique(c(manual_genes(), file_genes()))
    })

    # Placeholder function: implement actual GO -> genes retrieval
    get_go_genes_for <- function(go_id_or_name) {
      # TODO: Replace with real logic returning a character vector of genes.
      character()
    }

    # Build GO highlight list: list of lists(category, genes, color)
    go_highlights <- reactive({
      sel <- go_selection$chosen_go()
      if (!length(sel)) {
        return(NULL)
      }
      cols <- go_colors$chosen_colors()
      out <- lapply(sel, function(go_cat) {
        genes <- unique(get_go_genes_for(go_cat))
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
      # Remove NULLs
      out <- Filter(Negate(is.null), out)
      if (!length(out)) {
        return(NULL)
      }
      out
    })

    # Helper to subset by time point
    subset_timepoint <- function(tp) {
      reactive({
        if (is.null(datasets) || is.null(datasets$I)) {
          return(NULL)
        }
        df <- datasets$I
        if (!"Time_point" %in% names(df)) {
          return(NULL)
        }
        sub <- df[df$Time_point == tp, , drop = FALSE]
        if (!all(c("genes", "FC", "pval") %in% names(sub))) {
          return(NULL)
        }
        sub <- sub[!is.na(sub$FC) & !is.na(sub$pval), ]
        sub
      })
    }

    ds_STRESS_I <- subset_timepoint("STRESS_I")
    ds_STRESS_II <- subset_timepoint("STRESS_II")
    ds_RECOVERY_I <- subset_timepoint("RECOVERY_I")
    ds_RECOVERY_II <- subset_timepoint("RECOVERY_II")

    # Volcano modules with new separate highlight arguments
    volcano$server(
      "volcano_STRESS_I",
      dataset = ds_STRESS_I,
      go_highlights = go_highlights,
      custom_highlights = custom_genes,
      title = reactive("STRESS_I Volcano")
    )

    volcano$server(
      "volcano_STRESS_II",
      dataset = ds_STRESS_II,
      go_highlights = go_highlights,
      custom_highlights = custom_genes,
      title = reactive("STRESS_II Volcano")
    )

    volcano$server(
      "volcano_RECOVERY_I",
      dataset = ds_RECOVERY_I,
      go_highlights = go_highlights,
      custom_highlights = custom_genes,
      title = reactive("RECOVERY_I Volcano")
    )

    volcano$server(
      "volcano_RECOVERY_II",
      dataset = ds_RECOVERY_II,
      go_highlights = go_highlights,
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
