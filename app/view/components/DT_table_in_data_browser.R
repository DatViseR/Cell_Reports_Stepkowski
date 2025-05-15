# This module displays the interactive DT table in the data browser
# it takes dataset name, dataset path as the inputs. 
# the styles  match the around 500px width of the sidebar so the table is around 500px


# How it is used:

# # In your UI:
# DT_table_in_data_browser$ui(ns("Dataset_1_table"))

# # In server:
# DT_table_in_data_browser$server("Dataset_1_table", dataset = datasets()$I, table_caption = "Dataset I (Sidebar Table)")





#' @param id Shiny module id
#' @param dataset The data.frame to display
#' @param table_caption Optional table caption (string)
#' 
#' @export
box::use(
  shiny[moduleServer, NS, renderUI, uiOutput, tags, tagList],
  DT[datatable, renderDT, DTOutput]
)

# UI function
#' @export
ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$div(
      id = ns("sidebar_table_container"),
      style = "max-width: 500px; min-width: 300px; margin: 0 auto; overflow-x: auto; background: #fff; border-radius: 8px; box-shadow: 0 2px 8px rgba(0,0,0,0.03); padding: 12px;",
      DTOutput(ns("table"))
    )
  )
}

#' @export
server <- function(id, dataset, table_caption = NULL) {
  moduleServer(id, function(input, output, session) {
    # Ensure dataset is available
    output$table <- renderDT({
      req(dataset)  # This ensures the table only renders when dataset is available
      
      datatable(
        dataset,
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          autoWidth = TRUE,
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel'),
          columnDefs = list(list(width = '100px', targets = "_all"))
        ),
        extensions = 'Buttons',
        rownames = FALSE,
        caption = table_caption
      )
    })
  })
}