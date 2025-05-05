box::use(
  shiny[...]
  
)

#' @export
ui <- function(id) {
  ns <- NS(id)
# placeholder
  div(
    class = "CCCP_vs_Bortezomib-container",
    style = "padding: 20px;",
    
    h3("CCCP vs. Bortezomib", style = "color: #0062cc; margin-bottom: 20px;"),
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
