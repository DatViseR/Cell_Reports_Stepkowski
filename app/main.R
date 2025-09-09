box::use(
  shiny[
    bootstrapPage,
    div,
    moduleServer,
    NS,
    renderUI,
    tags,
    uiOutput,
    p,
    tagList
  ],
  bslib[page_navbar, navbar_options, nav_panel, nav_spacer, nav_menu, nav_item],
  here[here],
  data.table[fread],
  dplyr[glimpse],
  highcharter,
  app / view / About_module,
  app / view / Data_browser,
  app / view / Temporal_alterations,
  app / view / CCCP_vs_Bortezomib,
  app / view / Silencing_EEF1A1,
  app / view / THRONCAT,
  app / view / Meta_analysis,
  app / view / AI
)

#' @export
ui <- function(id) {
  ns <- NS(id)

  # Create a wrapper with the head tags first
  tagList(
    # Add the necessary JavaScript sources
    tags$head(
      tags$link(
        rel = "stylesheet",
        href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.15.4/css/all.min.css"
      ),
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
      nav_panel(
        title = "Dataset browser",
        Data_browser$ui(ns("dataset_browser"))
      ),
      nav_panel(
        title = "Temporal alterations",
        Temporal_alterations$ui(ns("temporal_alterations"))
      ),
      nav_panel(
        title = "CCCP vs. Bortezomib",
        CCCP_vs_Bortezomib$ui(ns("CCCP_vs_Bortezomib"))
      ),
      nav_panel(
        title = "Silencing EEF1A1 +-CCCP",
        Silencing_EEF1A1$ui(ns("Silencing_EEF1A1"))
      ),
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
    # Upload the GO data once globally as it will not be changed
    # and will be used in multiple modules

    GO <- arrow::read_parquet(here("data/GO.parquet"))
    # console log the uploaded data
    cat("GO data uploaded successfully\n")
    cat("GO data structure", "\n")
    glimpse(GO)

    # upload datasets
    dataset_1 <- arrow::read_parquet(here("data/Dataset_1_long.parquet"))
    cat("Dataset_1_cleaned uploaded successfully\n")
    cat("Dataset_1_structure", "\n")
    glimpse(dataset_1)
    cat("\n")

    dataset_2 <- arrow::read_parquet(here("data/Dataset_2.parquet"))
    cat("Dataset_2_uploaded successfully\n")
    cat("Dataset_2_structure", "\n")
    glimpse(dataset_2)
    cat("\n")

    dataset_3 <- arrow::read_parquet(here("data/Dataset_3.parquet"))
    cat("Dataset_3_uploaded successfully\n")
    cat("Dataset_3_structure", "\n")
    glimpse(dataset_3)
    cat("\n")

    dataset_4 <- arrow::read_parquet(here("data/Dataset_4.parquet"))
    cat("Dataset_4_uploaded successfully\n")
    cat("Dataset_4_structure", "\n")
    glimpse(dataset_4)
    cat("\n")

    dataset_5 <- data.table::fread(here("data/Dataset_5_cleaned.csv"))
    cat("Dataset_5_uploaded successfully\n")
    cat("Dataset_5_structure", "\n")
    glimpse(dataset_5)
    cat("\n")

    datasets = list(
      I = dataset_1,
      II = dataset_2,
      III = dataset_3,
      IV = dataset_4,
      V = dataset_5
    )

    cat("Datasets wrapped into list")

    # Initialize all module servers
    About_module$server("about")

    # Pass datasets to child module
    Data_browser$server("dataset_browser", datasets = datasets)

    Temporal_alterations$server(
      "temporal_alterations",
      GO = GO,
      datasets = datasets
    )

    CCCP_vs_Bortezomib$server(
      "CCCP_vs_Bortezomib",
      GO = GO,
      datasets = datasets
    )
    Silencing_EEF1A1$server("Silencing_EEF1A1")
    THRONCAT$server("THRONCAT")
    AI$server("AI")
    Meta_analysis$server("Meta_analysis")

    # Explicitly register highcharter dependencies
    session$sendCustomMessage(
      "js-eval",
      "if(typeof Highcharts === 'undefined') { console.error('Highcharts not loaded!'); }"
    )
  })
}
