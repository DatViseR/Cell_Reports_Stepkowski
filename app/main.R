box::use(
  shiny[bootstrapPage, div, moduleServer, NS, renderUI, tags, uiOutput, p, tagList],
  bslib[page_navbar, navbar_options, nav_panel, nav_spacer, nav_menu, nav_item],
  highcharter,  
  app/view/About_module,
  app/view/Data_browser,
  app/view/Temporal_alterations,
  app/view/CCCP_vs_Bortezomib,
  app/view/Silencing_EEF1A1,
  app/view/THRONCAT,
  app/view/Meta_analysis,
  app/view/AI
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  
  # Create a wrapper with the head tags first
  tagList(
    # Add the necessary JavaScript sources
    tags$head(
      tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.15.4/css/all.min.css"),
      tags$script(src = "https://code.highcharts.com/highcharts.js"),
      tags$script(src = "https://code.highcharts.com/modules/exporting.js"),
      tags$script(src = "https://code.highcharts.com/highcharts-more.js"),
      tags$script(src = "https://code.highcharts.com/modules/export-data.js"),
      tags$script(src = "https://code.highcharts.com/modules/accessibility.js")
    ),
    
    # Then add the page_navbar
    page_navbar(
      title = "Dynamic nascent proteome in mito-stress",
      navbar_options = navbar_options(
        bg = "#0062cc",
        underline = TRUE
      ),
      nav_panel(title = "About", About_module$ui(ns("about"))),
      nav_panel(title = "Dataset browser", Data_browser$ui(ns("dataset_browser"))),
      nav_panel(title = "Temporal alterations",Temporal_alterations$ui(ns("temporal_alterations"))),
      nav_panel(title = "CCCP vs. Bortezomib", CCCP_vs_Bortezomib$ui(ns("CCCP_vs_Bortezomib"))),
      nav_panel(title = "Silencing EEF1A1 +-CCCP", Silencing_EEF1A1$ui(ns("Silencing_EEF1A1"))),
      nav_panel(title = "THRONCAT", THRONCAT$ui(ns("THRONCAT"))),
      nav_panel(title = "Meta-analysis", Meta_analysis$ui(ns("Meta_analysis"))),
      nav_panel(title = "AI assisted analysis", AI$ui(ns("AI"))),
      
      nav_spacer(),
      nav_menu(
        title = "Links",
        align = "right",
        nav_item("article"),
        nav_item("source code"),
        nav_item("GitHub"),
        nav_item("LinkedIn of the developer")
      )
    )
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Initialize all module servers
    About_module$server("about")
    Data_browser$server("dataset_browser")
    Temporal_alterations$server("temporal_alterations")
    CCCP_vs_Bortezomib$server("CCCP_vs_Bortezomib")
    Silencing_EEF1A1$server("Silencing_EEF1A1")
    THRONCAT$server("THRONCAT")
    AI$server("AI")
    Meta_analysis$server("Meta_analysis")

    # Explicitly register highcharter dependencies
    session$sendCustomMessage("js-eval", "if(typeof Highcharts === 'undefined') { console.error('Highcharts not loaded!'); }")
  })
}