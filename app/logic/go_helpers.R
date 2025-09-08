# Helper functions for extracting genes from GO categories

box::use(
  dplyr[filter, pull]
)

#' Extract genes for selected GO categories
#' 
#' @param GO GO data frame with columns: id, name, genes
#' @param selected_go_categories Vector of GO category names
#' 
#' @export
get_genes_from_go_categories <- function(GO, selected_go_categories) {
  if (is.null(selected_go_categories) || length(selected_go_categories) == 0) {
    return(NULL)
  }
  
  if (is.null(GO)) {
    return(NULL)
  }
  
  # Filter GO data for selected categories and extract genes
  go_genes <- GO %>%
    filter(name %in% selected_go_categories) %>%
    pull(genes)
  
  # Handle case where genes might be stored as strings or lists
  if (length(go_genes) > 0) {
    # If genes are stored as comma-separated strings, split them
    if (is.character(go_genes)) {
      all_genes <- unlist(strsplit(go_genes, ","))
      all_genes <- trimws(all_genes)
    } else {
      # If genes are already a list/vector
      all_genes <- unlist(go_genes)
    }
    
    # Remove duplicates and empty strings
    all_genes <- unique(all_genes[all_genes != ""])
    
    return(all_genes)
  }
  
  return(NULL)
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