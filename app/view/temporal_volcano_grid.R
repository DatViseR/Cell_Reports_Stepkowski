# This module creates a 2x2 grid of volcano plots for temporal analysis
# showing the four time points: STRESS I, STRESS II, RECOVERY I, RECOVERY II

box::use(
  shiny[moduleServer, NS, div, h4, p, br, fluidRow, column, reactive, req, isolate],
  bslib[card, card_header, card_body],
  dplyr[filter, select, rename, mutate],
  htmltools[tagList],
  app/view/volcano
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  
  card(
    card_header(
      h4("Temporal Volcano Plots - Dataset I", style = "margin: 0;")
    ),
    card_body(
      p("Volcano plots showing differential expression across four time points during CCCP treatment and recovery:"),
      br(),
      
      # 2x2 grid of volcano plots
      fluidRow(
        column(
          width = 6,
          div(
            style = "border: 1px solid #dee2e6; border-radius: 0.375rem; padding: 10px; margin-bottom: 15px;",
            h4("STRESS I (0-30 min)", style = "text-align: center; margin-bottom: 15px; color: #d73027;"),
            volcano$ui(ns("volcano_stress_i"), height = "400px")
          )
        ),
        column(
          width = 6,
          div(
            style = "border: 1px solid #dee2e6; border-radius: 0.375rem; padding: 10px; margin-bottom: 15px;",
            h4("STRESS II (30-60 min)", style = "text-align: center; margin-bottom: 15px; color: #f46d43;"),
            volcano$ui(ns("volcano_stress_ii"), height = "400px")
          )
        )
      ),
      
      fluidRow(
        column(
          width = 6,
          div(
            style = "border: 1px solid #dee2e6; border-radius: 0.375rem; padding: 10px;",
            h4("RECOVERY I (60-90 min)", style = "text-align: center; margin-bottom: 15px; color: #74add1;"),
            volcano$ui(ns("volcano_recovery_i"), height = "400px")
          )
        ),
        column(
          width = 6,
          div(
            style = "border: 1px solid #dee2e6; border-radius: 0.375rem; padding: 10px;",
            h4("RECOVERY II (90-120 min)", style = "text-align: center; margin-bottom: 15px; color: #4575b4;"),
            volcano$ui(ns("volcano_recovery_ii"), height = "400px")
          )
        )
      ),
      
      br(),
      div(
        style = "font-size: 90%; color: #666; text-align: center;",
        "Each plot shows log2(fold change) vs -log10(p-value). Points are colored by significance status.",
        br(),
        "Highlighted genes represent selected GO categories or custom annotations."
      )
    )
  )
}

#' @export
server <- function(id, 
                   dataset_1, 
                   highlight_genes = reactive(NULL),
                   go_genes = reactive(NULL),
                   custom_genes = reactive(NULL),
                   fc_cutoff = reactive(1),
                   pval_cutoff = reactive(0.05)) {
  moduleServer(id, function(input, output, session) {
    
    # Prepare datasets for each time point
    stress_i_data <- reactive({
      req(dataset_1())
      
      tryCatch({
        data <- dataset_1()
        
        # Check if required columns exist
        required_cols <- c("Gene_single", "Time_point", "p_value", "log2(Fold)", "q_value")
        missing_cols <- required_cols[!required_cols %in% names(data)]
        
        if (length(missing_cols) > 0) {
          cat("Warning: Missing columns in dataset:", paste(missing_cols, collapse = ", "), "\n")
          return(NULL)
        }
        
        # Filter for STRESS_I time point and prepare columns
        stress_data <- data %>%
          filter(Time_point == "STRESS_I") %>%
          select(Gene_single, p_value, `log2(Fold)`, q_value) %>%
          rename(
            genes = Gene_single,
            pval = p_value,
            log2FC = `log2(Fold)`,
            qvalue = q_value
          ) %>%
          # Convert p_value from -log10 back to regular p-value
          mutate(
            # p_value column contains -log10(p-value), convert back to p-value
            pval = 10^(-pval),
            # Create FC column for volcano module compatibility 
            FC = 2^log2FC
          ) %>%
          # Remove rows with missing data
          filter(!is.na(genes) & !is.na(pval) & !is.na(FC))
        
        if (nrow(stress_data) == 0) {
          cat("Warning: No data found for STRESS_I time point\n")
          return(NULL)
        }
        
        cat("STRESS_I data prepared:", nrow(stress_data), "genes\n")
        return(stress_data)
      }, error = function(e) {
        cat("Error preparing STRESS_I data:", e$message, "\n")
        return(NULL)
      })
    })
    
    stress_ii_data <- reactive({
      req(dataset_1())
      
      tryCatch({
        data <- dataset_1()
        
        stress_data <- data %>%
          filter(Time_point == "STRESS_II") %>%
          select(Gene_single, p_value, `log2(Fold)`, q_value) %>%
          rename(
            genes = Gene_single,
            pval = p_value,
            log2FC = `log2(Fold)`,
            qvalue = q_value
          ) %>%
          mutate(
            # p_value column contains -log10(p-value), convert back to p-value
            pval = 10^(-pval),
            # Create FC column for volcano module compatibility 
            FC = 2^log2FC
          ) %>%
          filter(!is.na(genes) & !is.na(pval) & !is.na(FC))
        
        if (nrow(stress_data) == 0) {
          cat("Warning: No data found for STRESS_II time point\n")
          return(NULL)
        }
        
        cat("STRESS_II data prepared:", nrow(stress_data), "genes\n")
        return(stress_data)
      }, error = function(e) {
        cat("Error preparing STRESS_II data:", e$message, "\n")
        return(NULL)
      })
    })
    
    recovery_i_data <- reactive({
      req(dataset_1())
      
      tryCatch({
        data <- dataset_1()
        
        recovery_data <- data %>%
          filter(Time_point == "RECOVERY_I") %>%
          select(Gene_single, p_value, `log2(Fold)`, q_value) %>%
          rename(
            genes = Gene_single,
            pval = p_value,
            log2FC = `log2(Fold)`,
            qvalue = q_value
          ) %>%
          mutate(
            # p_value column contains -log10(p-value), convert back to p-value
            pval = 10^(-pval),
            # Create FC column for volcano module compatibility 
            FC = 2^log2FC
          ) %>%
          filter(!is.na(genes) & !is.na(pval) & !is.na(FC))
        
        if (nrow(recovery_data) == 0) {
          cat("Warning: No data found for RECOVERY_I time point\n")
          return(NULL)
        }
        
        cat("RECOVERY_I data prepared:", nrow(recovery_data), "genes\n")
        return(recovery_data)
      }, error = function(e) {
        cat("Error preparing RECOVERY_I data:", e$message, "\n")
        return(NULL)
      })
    })
    
    recovery_ii_data <- reactive({
      req(dataset_1())
      
      tryCatch({
        data <- dataset_1()
        
        recovery_data <- data %>%
          filter(Time_point == "RECOVERY_II") %>%
          select(Gene_single, p_value, `log2(Fold)`, q_value) %>%
          rename(
            genes = Gene_single,
            pval = p_value,
            log2FC = `log2(Fold)`,
            qvalue = q_value
          ) %>%
          mutate(
            # p_value column contains -log10(p-value), convert back to p-value
            pval = 10^(-pval),
            # Create FC column for volcano module compatibility 
            FC = 2^log2FC
          ) %>%
          filter(!is.na(genes) & !is.na(pval) & !is.na(FC))
        
        if (nrow(recovery_data) == 0) {
          cat("Warning: No data found for RECOVERY_II time point\n")
          return(NULL)
        }
        
        cat("RECOVERY_II data prepared:", nrow(recovery_data), "genes\n")
        return(recovery_data)
      }, error = function(e) {
        cat("Error preparing RECOVERY_II data:", e$message, "\n")
        return(NULL)
      })
    })
    
    # Combine highlight genes from GO and custom annotations
    combined_highlight_genes <- reactive({
      go_genes_list <- go_genes()
      custom_genes_list <- custom_genes()
      
      # Combine both lists
      all_genes <- c(go_genes_list, custom_genes_list)
      
      # Remove duplicates and return
      if (length(all_genes) > 0) {
        return(unique(all_genes))
      } else {
        return(NULL)
      }
    })
    
    # Initialize volcano plot servers for each time point
    volcano_stress_i <- volcano$server(
      "volcano_stress_i",
      dataset = stress_i_data,
      highlight_genes = combined_highlight_genes,
      fc_cutoff = fc_cutoff,
      pval_cutoff = pval_cutoff,
      title = reactive("STRESS I (0-30 min)")
    )
    
    volcano_stress_ii <- volcano$server(
      "volcano_stress_ii",
      dataset = stress_ii_data,
      highlight_genes = combined_highlight_genes,
      fc_cutoff = fc_cutoff,
      pval_cutoff = pval_cutoff,
      title = reactive("STRESS II (30-60 min)")
    )
    
    volcano_recovery_i <- volcano$server(
      "volcano_recovery_i",
      dataset = recovery_i_data,
      highlight_genes = combined_highlight_genes,
      fc_cutoff = fc_cutoff,
      pval_cutoff = pval_cutoff,
      title = reactive("RECOVERY I (60-90 min)")
    )
    
    volcano_recovery_ii <- volcano$server(
      "volcano_recovery_ii",
      dataset = recovery_ii_data,
      highlight_genes = combined_highlight_genes,
      fc_cutoff = fc_cutoff,
      pval_cutoff = pval_cutoff,
      title = reactive("RECOVERY II (90-120 min)")
    )
    
    # Return selected genes from all plots (for potential future use)
    return(list(
      selected_genes_stress_i = volcano_stress_i$selected_genes,
      selected_genes_stress_ii = volcano_stress_ii$selected_genes,
      selected_genes_recovery_i = volcano_recovery_i$selected_genes,
      selected_genes_recovery_ii = volcano_recovery_ii$selected_genes
    ))
  })
}