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
  app / view / volcano,
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
              volcano$ui(ns("volcano_CCCP_DMSO"))
            ),
            column(
              width = 6,
              div(
                tags$b("Bortezomib vs. DMSO"),
                style = "text-align: center; margin-top: 5px; color: #2ca02c;"
              ),
              volcano$ui(ns("volcano_Bortezomib_DMSO"))
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
    go_selection <- GO_selection_module$server("go_selection_cccp_bortezomib", GO = GO)
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

    # Transform Dataset II data for volcano module compatibility
    transformed_dataset <- reactive({
      if (is.null(datasets) || is.null(datasets$II)) {
        return(NULL)
      }
      
      df <- datasets$II
      
      # Create two rows per gene - one for each comparison
      cccp_data <- data.frame(
        Time_point = "CCCP_DMSO",
        Gene_names = df$Gene_names,
        Leading_razor_protein = df$Leading_razor_protein,
        Protein_names = df$Protein_names,
        `log2(Fold)` = df$Log2FC_CCCP_DMSO,
        q_value = df$q_value_CCCP_DMSO,
        p_value_neg_log10 = df$`-Log10_p-value_CCCP_DMSO`,
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
      
      bortezomib_data <- data.frame(
        Time_point = "Bortezomib_DMSO",
        Gene_names = df$Gene_names,
        Leading_razor_protein = df$Leading_razor_protein,
        Protein_names = df$Protein_names,
        `log2(Fold)` = df$Log2FC_Bortezomib_DMSO,
        q_value = df$q_value_Bortezomib_DMSO,
        p_value_neg_log10 = df$`-Log10_p-value_Bortezomib_DMSO`,
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
      
      # Combine the data
      combined_data <- rbind(cccp_data, bortezomib_data)
      
      # Remove rows with missing critical values
      combined_data <- combined_data[
        !is.na(combined_data$`log2(Fold)`) & 
        !is.na(combined_data$q_value) &
        !is.na(combined_data$Gene_names)
      ]
      
      cat("Transformed dataset - rows:", nrow(combined_data), "\n")
      cat("CCCP_DMSO rows:", sum(combined_data$Time_point == "CCCP_DMSO"), "\n")
      cat("Bortezomib_DMSO rows:", sum(combined_data$Time_point == "Bortezomib_DMSO"), "\n")
      
      return(combined_data)
    })

    # Volcano modules for the two comparisons
    volcano$server(
      "volcano_CCCP_DMSO",
      dataset = transformed_dataset,
      timepoint = "CCCP_DMSO",
      go_annotations = go_highlights,
      custom_highlights = custom_genes,
      title = reactive("CCCP vs. DMSO")
    )

    volcano$server(
      "volcano_Bortezomib_DMSO",
      dataset = transformed_dataset,
      timepoint = "Bortezomib_DMSO",
      go_annotations = go_highlights,
      custom_highlights = custom_genes,
      title = reactive("Bortezomib vs. DMSO")
    )
  })
}
