box::use(
  shiny[moduleServer, NS, reactive, is.reactive, req, validate, need],
  dplyr[filter, pull, distinct]
)

#' GO Gene Mapper Module
#'
#' This module provides functionality to map GO categories to gene symbols
#' using the GO annotation data.
#'
#' @export
ui <- function(id) {
  # This module doesn't need UI - it's a data service module
  NULL
}

#' GO Gene Mapper Server
#'
#' @param id Module ID
#' @param GO_data Reactive or static data frame with GO annotations
#'   Expected columns: id, name, gene, ontology
#'
#' @export
server <- function(id, GO_data) {
  moduleServer(id, function(input, output, session) {
    # Ensure GO_data is reactive
    GO_reactive <- if (is.reactive(GO_data)) {
      GO_data
    } else {
      reactive(GO_data)
    }

    # Function to get genes for a given GO term (by ID or name)
    get_genes_for_go <- function(go_term) {
      tryCatch(
        {
          go_df <- GO_reactive()

          if (is.null(go_df) || nrow(go_df) == 0) {
            return(character(0))
          }

          # Search by both ID and name (case-insensitive for name)
          genes <- go_df %>%
            filter(
              id == go_term |
                tolower(name) == tolower(go_term)
            ) %>%
            pull(gene) %>%
            unique()

          # Remove any NA or empty values
          genes <- genes[!is.na(genes) & genes != ""]

          return(genes)
        },
        error = function(e) {
          warning(paste(
            "Error in get_genes_for_go for term",
            go_term,
            ":",
            e$message
          ))
          return(character(0))
        }
      )
    }

    # Function to search GO terms by partial name match
    search_go_terms <- function(search_term) {
      tryCatch(
        {
          go_df <- GO_reactive()

          if (is.null(go_df) || nrow(go_df) == 0) {
            return(data.frame(
              id = character(0),
              name = character(0),
              gene_count = numeric(0)
            ))
          }

          # Search for terms containing the search string (case-insensitive)
          matches <- go_df %>%
            filter(grepl(search_term, name, ignore.case = TRUE)) %>%
            group_by(id, name) %>%
            summarise(gene_count = n_distinct(gene), .groups = "drop") %>%
            arrange(desc(gene_count))

          return(matches)
        },
        error = function(e) {
          warning(paste(
            "Error in search_go_terms for",
            search_term,
            ":",
            e$message
          ))
          return(data.frame(
            id = character(0),
            name = character(0),
            gene_count = numeric(0)
          ))
        }
      )
    }

    # Return the functions for use by parent modules
    return(list(
      get_genes_for_go = get_genes_for_go,
      search_go_terms = search_go_terms,
      go_data = GO_reactive
    ))
  })
}
