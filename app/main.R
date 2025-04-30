box::use(
  shiny[bootstrapPage, div, moduleServer, NS, renderUI, tags, uiOutput, p],
  bslib[page_navbar, navbar_options, nav_panel, nav_spacer, nav_menu, nav_item],
  app/view/About_module,
  app/view/Data_browser,
  app/view/Temporal_alterations,
 
  
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  # Load the font awesome stylesheet

  
  
  page_navbar(
    title = "Dynamic nascent proteome in mito-stress",
    navbar_options = navbar_options(
      bg = "#0062cc",
      underline = TRUE
    ),
    nav_panel(title = "About", About_module$ui(ns("about"))),
    nav_panel(title = "Dataset browser", Data_browser$ui(ns("dataset_browser"))),
    nav_panel(title = "Temporal alterations",Temporal_alterations$ui(ns("temporal_alterations"))),
    #nav_panel(title = "CCCP vs. Bortezomib", CCCP_vs_Bortezomib$ui(ns("CCCP_vs_Bortezomib"))),
    #nav_panel(title = "Silencing EEF1A1 +-CCCP", Silencing_EEF1A1$ui(ns("Silencing_EEF1A1"))),
    #nav_panel(title = "THRONCAT", THRONCAT$ui(ns("THRONCAT"))), 
    #nav_panel(title = "Meta-analysis", Meta_analysis$ui(ns("Meta_analysis"))),
   
    nav_spacer(),
    nav_menu(
      title = "Links",
      align = "right",
      nav_item("article"),
      nav_item("source code"),
      nav_item("GitHub"),
      nav_item("LinkedIn of the developer"),
      
    )
  )
  
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    output$message <- renderUI({
      div(
        style = "display: flex; justify-content: center; align-items: center; height: 100vh;",
        tags$h1(
          tags$a("Check out Rhino docs!", href = "https://appsilon.github.io/rhino/")
        )
      )
    })
  })
}
