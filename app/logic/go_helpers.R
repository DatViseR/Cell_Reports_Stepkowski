# Helper functions for extracting genes from GO categories

box::use(
  dplyr[filter, pull]
)

#' Extract genes for selected GO categories
#' 
#' @param GO GO data frame with columns: id, name, genes (or similar structure)
#' @param selected_go_categories Vector of GO category names
#' 
#' @export
get_genes_from_go_categories <- function(GO, selected_go_categories) {
  if (is.null(selected_go_categories) || length(selected_go_categories) == 0) {
    return(NULL)
  }
  
  if (is.null(GO) || nrow(GO) == 0) {
    return(NULL)
  }
  
  tryCatch({
    # Filter GO data for selected categories and extract genes
    go_subset <- GO %>%
      filter(name %in% selected_go_categories)
    
    if (nrow(go_subset) == 0) {
      cat("No matching GO categories found\n")
      return(NULL)
    }
    
    # Extract genes - handle different possible column names
    if ("genes" %in% names(go_subset)) {
      go_genes <- go_subset %>% pull(genes)
    } else if ("gene" %in% names(go_subset)) {
      go_genes <- go_subset %>% pull(gene)
    } else if ("gene_symbol" %in% names(go_subset)) {
      go_genes <- go_subset %>% pull(gene_symbol)
    } else {
      cat("No gene column found in GO data\n")
      return(NULL)
    }
    
    # Handle case where genes might be stored as strings or lists
    if (length(go_genes) > 0) {
      # If genes are stored as comma-separated strings, split them
      if (is.character(go_genes)) {
        all_genes <- unlist(strsplit(go_genes, "[,;\\s]+"))
        all_genes <- trimws(all_genes)
      } else {
        # If genes are already a list/vector
        all_genes <- unlist(go_genes)
      }
      
      # Remove duplicates and empty strings
      all_genes <- unique(all_genes[all_genes != "" & !is.na(all_genes)])
      
      if (length(all_genes) > 0) {
        cat("Extracted", length(all_genes), "genes from", length(selected_go_categories), "GO categories\n")
        return(all_genes)
      }
    }
    
    return(NULL)
  }, error = function(e) {
    cat("Error extracting genes from GO categories:", e$message, "\n")
    return(NULL)
  })
}

#' Create highlight info for GO categories
#' 
#' @param selected_go_categories Vector of GO category names
#' 
#' @export
create_go_highlight_info <- function(selected_go_categories) {
  if (is.null(selected_go_categories) || length(selected_go_categories) == 0) {
    return(NULL)
  }
  
  return(list(
    name = paste(selected_go_categories, collapse = ", "),
    id = "GO Categories",
    type = "GO"
  ))
}