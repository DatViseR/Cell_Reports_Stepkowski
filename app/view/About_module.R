box::use(
  shiny[div, h1, h2, h3, h4, p, a, br, strong, em, HTML, moduleServer, NS, 
        tags, tabPanel, fluidRow, column],
  bslib[card, card_header, card_body]
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  
  div(
    class = "about-container",
    style = "padding: 20px;",
    
    h1("About This Interactive Resource", 
       style = "color: #0062cc; margin-bottom: 20px;"),
    
    card(
      card_header(
        h2("Paper Information", style = "margin: 0;")
      ),
      card_body(
        h3("Stępkowski et al. Cell Reports 2024"),
        p(
          "This interactive tool provides resources and visualization capabilities for the data 
          presented in our Cell Reports paper. The tool allows exploration of the dataset 
          through custom visualization and filtering options."
        ),
        h4("Citation"),
        p(
          HTML("Stępkowski, T. et al. (2024). <em>Title of the paper</em>. 
               Cell Reports, Volume(Issue), pages. 
               <a href='https://doi.org/DOI_NUMBER'>https://doi.org/DOI_NUMBER</a>")
        )
      )
    ),
    
    br(),
    
    card(
      card_header(
        h2("Authors", style = "margin: 0;")
      ),
      card_body(
        fluidRow(
          column(
            width = 6,
            h4("Research Team"),
            tags$ul(
              tags$li("Author One, PhD - Research Institute"),
              tags$li("Author Two, MD - Medical University"),
              tags$li("Author Three, PhD - University Department")
            )
          ),
          column(
            width = 6,
            h4("Contact"),
            p(
              "For questions about the data or tool, please contact:",
              br(),
              a("corresponding.author@institution.edu", 
                href = "mailto:corresponding.author@institution.edu")
            )
          )
        )
      )
    ),
    
    br(),
    
    card(
      card_header(
        h2("Tool Information", style = "margin: 0;")
      ),
      card_body(
        p(
          "This interactive resource tool was built using R Shiny and the Rhino framework 
          by Appsilon. It allows for exploration of the dataset presented in our publication 
          through various visualizations and filtering options."
        ),
        h4("How to Use"),
        p(
          "Navigate through the tabs to access different analyses and visualizations. 
          The 'Dataset browser' tab allows custom exploration of the data with filtering options."
        ),
        h4("Acknowledgments"),
        p(
          "This work was supported by [funding sources]. 
          We thank [acknowledgments for technical support, etc.]."
        )
      )
    )
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Server logic for the About module
    # (likely minimal for an About page)
  })
}