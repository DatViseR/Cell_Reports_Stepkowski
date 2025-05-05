box::use(
  shiny[...]
  
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  # placeholder
  div(
    class = "Silencing_EEF1A1-container",
    style = "padding: 20px;",
    
    h3("Silencing EEF1A1", style = "color: #0062cc; margin-bottom: 20px;"),
    p("Explore the datasets from Stępkowski et al. Cell Reports 2024. Select a dataset using the tabs below."),
    br()
  )

  
}

#' @export

server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Placeholder for server logic
  })
}
