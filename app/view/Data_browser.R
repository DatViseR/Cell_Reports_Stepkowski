# This module only displays the tabset for V datasets 

box::use(
  shiny[div, h3, h4, p, br, moduleServer, NS, tabsetPanel, tabPanel, fluidRow, column, 
        verbatimTextOutput, uiOutput, renderUI, observeEvent, reactive, req, h5],
  bslib[card, card_header, card_body, layout_sidebar, sidebar],
  DT[datatable, renderDT, DTOutput],
  # Import our dataset description module
  app/view/components/Dataset_Description[create_description],
  app/view/components/Constants[ui_info_for_tabPanel],
  here[here],

  app/view/components/DT_table_in_data_browser
  
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
    # info instead of tabPanel titles  
    
    
      # Dataset I
      tabPanel(
        "Dataset I",
        br(),
        layout_sidebar(
          sidebar = sidebar(
            width = 500, # Increased width from 300 to 400
          
            # defined in constants
            title = ui_info_for_tabPanel(),
                 
                     
            
            # Use the updated dataset description component
            create_description("I"),
            
            # Placeholder for filters
            div(
              style = "margin-top: 15px; border-top: 1px solid #eee; padding-top: 10px;",
              h4("Data Filters", style = "font-size: 0.95rem; color: #333;"),
              p("Filters will be implemented here", style = "font-size: 0.85rem;")
            )
          ),
          
          # Main content - Interactive DT table
          card(
            card_header(
              h3("Dataset I Data", style = "margin: 0;")
            ),
            card_body(
              DT_table_in_data_browser$ui(ns("Dataset_1_table"))
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
            width = 500, # Increased width
            title = ui_info_for_tabPanel(),
            
            # Use the updated dataset description component
            create_description("II"),
            
            # Placeholder for filters
            div(
              style = "margin-top: 15px; border-top: 1px solid #eee; padding-top: 10px;",
              h4("Data Filters", style = "font-size: 0.95rem; color: #333;"),
              p("Filters will be implemented here", style = "font-size: 0.85rem;")
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
            width = 500, # Increased width
            title = ui_info_for_tabPanel(),
            
            # Use the updated dataset description component
            create_description("III"),
            
            # Placeholder for filters
            div(
              style = "margin-top: 15px; border-top: 1px solid #eee; padding-top: 10px;",
              h4("Data Filters", style = "font-size: 0.95rem; color: #333;"),
              p("Filters will be implemented here", style = "font-size: 0.85rem;")
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
            width = 500, # Increased width
            title = ui_info_for_tabPanel(),
            
            # Use the updated dataset description component
            create_description("IV"),
            
            # Placeholder for filters
            div(
              style = "margin-top: 15px; border-top: 1px solid #eee; padding-top: 10px;",
              h4("Data Filters", style = "font-size: 0.95rem; color: #333;"),
              p("Filters will be implemented here", style = "font-size: 0.85rem;")
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
            width = 500, # Increased width
            title = ui_info_for_tabPanel(),
            
            # Use the updated dataset description component
            create_description("V"),
            
            # Placeholder for filters
            div(
              style = "margin-top: 15px; border-top: 1px solid #eee; padding-top: 10px;",
              h4("Data Filters", style = "font-size: 0.95rem; color: #333;"),
              p("Filters will be implemented here", style = "font-size: 0.85rem;")
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
   DT_table_in_data_browser$server("Dataset_1_table", dataset = datasets$I  , table_caption = "Dataset I (Sidebar Table)")
   # DT_table_in_data_browser$server("table_I", dataset = datasets$I, table_caption = "Dataset I - Interactive Table")
   # DT_table_in_data_browser$server("table_II", dataset = datasets$II, table_caption = "Dataset II - Interactive Table")
   # DT_table_in_data_browser$server("table_III", dataset = datasets$III, table_caption = "Dataset III - Interactive Table")
   # DT_table_in_data_browser$server("table_IV", dataset = datasets$IV, table_caption = "Dataset IV - Interactive Table")
   # DT_table_in_data_browser$server("table_V", dataset = datasets$V, table_caption = "Dataset V - Interactive Table")
   # 
   #  
    # Tab switching logic
    observeEvent(input$dataset_tabs, {
      # You can add specific actions when tabs are switched
      # For example, you might want to reload data or update filters
    })
  })
}
