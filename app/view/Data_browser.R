# This module only displays the tabset for V datasets 

box::use(
  shiny[div, h3, h4, p, br, moduleServer, NS, tabsetPanel, tabPanel, fluidRow, column, 
        verbatimTextOutput, uiOutput, renderUI, observeEvent, reactive, req],
  bslib[card, card_header, card_body, layout_sidebar, sidebar],
  DT[datatable, renderDT, DTOutput]
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  
  div(
    class = "data-browser-container",
    style = "padding: 20px;",
    
    h3("Dataset Browser", style = "color: #0062cc; margin-bottom: 20px;"),
    p("Explore the datasets from Stępkowski et al. Cell Reports 2024. Select a dataset using the tabs below."),
    br(),
    
    tabsetPanel(
      id = ns("dataset_tabs"),
      
      # Dataset I
      tabPanel(
        "Dataset I",
        br(),
        layout_sidebar(
          sidebar = sidebar(
            width = 300,
            title = "Dataset I Description",
            
            # Placeholder for Dataset Description Module
            div(
              id = ns("dataset_I_description"),
              h4("About Dataset I"),
              p("This dataset contains information about..."),
              p("Number of samples: XXX"),
              p("Variables: XXX"),
              p("Methodology: XXX"),
              
              # Placeholder for filters or additional controls
              h4("Data Filters"),
              p("Filters will be implemented here")
            )
          ),
          
          # Main content - Interactive DT table
          card(
            card_header(
              h3("Dataset I Data", style = "margin: 0;")
            ),
            card_body(
              DTOutput(ns("table_I"))
            )
          )
        )
      ),
      
      # Dataset II
      tabPanel(
        "Dataset II",
        br(),
        layout_sidebar(
          sidebar = sidebar(
            width = 300,
            title = "Dataset II Description",
            
            # Placeholder for Dataset Description Module
            div(
              id = ns("dataset_II_description"),
              h4("About Dataset II"),
              p("This dataset contains information about..."),
              p("Number of samples: XXX"),
              p("Variables: XXX"),
              p("Methodology: XXX"),
              
              # Placeholder for filters or additional controls
              h4("Data Filters"),
              p("Filters will be implemented here")
            )
          ),
          
          # Main content - Interactive DT table
          card(
            card_header(
              h3("Dataset II Data", style = "margin: 0;")
            ),
            card_body(
              DTOutput(ns("table_II"))
            )
          )
        )
      ),
      
      # Dataset III
      tabPanel(
        "Dataset III",
        br(),
        layout_sidebar(
          sidebar = sidebar(
            width = 300,
            title = "Dataset III Description",
            
            # Placeholder for Dataset Description Module
            div(
              id = ns("dataset_III_description"),
              h4("About Dataset III"),
              p("This dataset contains information about..."),
              p("Number of samples: XXX"),
              p("Variables: XXX"),
              p("Methodology: XXX"),
              
              # Placeholder for filters or additional controls
              h4("Data Filters"),
              p("Filters will be implemented here")
            )
          ),
          
          # Main content - Interactive DT table
          card(
            card_header(
              h3("Dataset III Data", style = "margin: 0;")
            ),
            card_body(
              DTOutput(ns("table_III"))
            )
          )
        )
      ),
      
      # Dataset IV
      tabPanel(
        "Dataset IV",
        br(),
        layout_sidebar(
          sidebar = sidebar(
            width = 300,
            title = "Dataset IV Description",
            
            # Placeholder for Dataset Description Module
            div(
              id = ns("dataset_IV_description"),
              h4("About Dataset IV"),
              p("This dataset contains information about..."),
              p("Number of samples: XXX"),
              p("Variables: XXX"),
              p("Methodology: XXX"),
              
              # Placeholder for filters or additional controls
              h4("Data Filters"),
              p("Filters will be implemented here")
            )
          ),
          
          # Main content - Interactive DT table
          card(
            card_header(
              h3("Dataset IV Data", style = "margin: 0;")
            ),
            card_body(
              DTOutput(ns("table_IV"))
            )
          )
        )
      ),
      
      # Dataset V
      tabPanel(
        "Dataset V",
        br(),
        layout_sidebar(
          sidebar = sidebar(
            width = 300,
            title = "Dataset V Description",
            
            # Placeholder for Dataset Description Module
            div(
              id = ns("dataset_V_description"),
              h4("About Dataset V"),
              p("This dataset contains information about..."),
              p("Number of samples: XXX"),
              p("Variables: XXX"),
              p("Methodology: XXX"),
              
              # Placeholder for filters or additional controls
              h4("Data Filters"),
              p("Filters will be implemented here")
            )
          ),
          
          # Main content - Interactive DT table
          card(
            card_header(
              h3("Dataset V Data", style = "margin: 0;")
            ),
            card_body(
              DTOutput(ns("table_V"))
            )
          )
        )
      )
    )
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Placeholder for loading datasets
    datasets <- reactive({
      list(
        I = data.frame(
          Variable1 = c("Sample1", "Sample2", "Sample3"),
          Variable2 = c(10, 20, 30),
          Variable3 = c("Type A", "Type B", "Type A")
        ),
        II = data.frame(
          Variable1 = c("Sample1", "Sample2", "Sample3"),
          Variable2 = c(15, 25, 35),
          Variable3 = c("Type C", "Type D", "Type C")
        ),
        III = data.frame(
          Variable1 = c("Sample1", "Sample2", "Sample3"),
          Variable2 = c(40, 50, 60),
          Variable3 = c("Type E", "Type F", "Type E")
        ),
        IV = data.frame(
          Variable1 = c("Sample1", "Sample2", "Sample3"),
          Variable2 = c(70, 80, 90),
          Variable3 = c("Type G", "Type H", "Type G")
        ),
        V = data.frame(
          Variable1 = c("Sample1", "Sample2", "Sample3"),
          Variable2 = c(100, 110, 120),
          Variable3 = c("Type I", "Type J", "Type I")
        )
      )
    })
    
    # Render tables
    output$table_I <- renderDT({
      datatable(
        datasets()$I,
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel')
        ),
        extensions = 'Buttons',
        rownames = FALSE,
        caption = "Dataset I - Interactive Table"
      )
    })
    
    output$table_II <- renderDT({
      datatable(
        datasets()$II,
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel')
        ),
        extensions = 'Buttons',
        rownames = FALSE,
        caption = "Dataset II - Interactive Table"
      )
    })
    
    output$table_III <- renderDT({
      datatable(
        datasets()$III,
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel')
        ),
        extensions = 'Buttons',
        rownames = FALSE,
        caption = "Dataset III - Interactive Table"
      )
    })
    
    output$table_IV <- renderDT({
      datatable(
        datasets()$IV,
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel')
        ),
        extensions = 'Buttons',
        rownames = FALSE,
        caption = "Dataset IV - Interactive Table"
      )
    })
    
    output$table_V <- renderDT({
      datatable(
        datasets()$V,
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel')
        ),
        extensions = 'Buttons',
        rownames = FALSE,
        caption = "Dataset V - Interactive Table"
      )
    })
    
    # Tab switching logic
    observeEvent(input$dataset_tabs, {
      # You can add specific actions when tabs are switched
      # For example, you might want to reload data or update filters
    })
  })
}
