box::use(
  shiny[div, h1, h2, h3, h4, p, a, br, strong, em, HTML, moduleServer, NS, img, 
        tags, tabPanel, fluidRow, column, icon, span],
  bslib[card, card_header, card_body, accordion, accordion_panel, value_box, layout_column_wrap],
  
  app/view/components/DataSet_Accordion[datasets_accordion]
  
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  

  div(
    class = "about-container",
    style = "padding: 20px;",
    
    # Simplified Hero section using more Shiny components
    fluidRow(
      column(
        width = 12,
        card(
          card_body(
            fluidRow( style = "padding: 20px;",
              # Left content area (text)
            column(style = "padding: 20px;", 
  width = 8,
  h1("Interactive Data Science Tool for Temporal Alterations of the Nascent Proteome in Response to Mitochondrial Stress", 
     style = "color: #0062cc; margin-bottom: 24px; font-weight: 600;"),   
  
  p(
    "This interactive application provides comprehensive analysis tools for exploring proteomic resources described in ",
    tags$a(
      href = "https://doi.org/10.1016/j.celrep.2024.114803",
      "Stępkowski et al. Cell Reports 2024"
    ),
    ". Our tool enables researchers to explore, visualize, gain new insights and answer specific questions related to the 
     interplay between mitochondrial stress, translation attenuation and their impact on the composition of the nascent proteome."
  ),

                # Citation box - keeping custom CSS
                div(
                  style = "background: linear-gradient(to right, #f8f9fa, #ffffff); border-left: 4px solid #0062cc; 
                     padding: 18px; margin: 40px 0; border-radius: 0 8px 8px 0;",
                  strong("This application acompanies: ", style = "color: #333;"),
                  p(
                    style = "margin: 6px 0 0 0;",
                    a(
                      "Stępkowski TM, Linke V, Stadnik D, Zakrzewski M, Zawada AE, Serwa RA, Chacinska A. Temporal alterations of the nascent proteome in response to mitochondrial stress. Cell Rep. 2024 Oct 22;43(10):114803", 
                      href =  "https://doi.org/10.1016/j.celrep.2024.114803",
                      style = "color: #0062cc; text-decoration: none; font-weight: 500;"
                    )
                  )
                ),
                
                # Stats tiles - keeping custom styling
                fluidRow(
                  column(
                    width = 4,
                    div(
                      style = "background-color: #f1f7ff; padding: 15px; border-radius: 8px; text-align: center;",
                      h4("4 datasets + meta-analysis", style = "color: #0062cc; margin: 0; font-size: 22px; font-weight: 600;"),
                      p("explore, analyze , build new hypotheses", style = "margin: 5px 0 0 0; color: #555;")
                    )
                  ),
                  column(
                    width = 4,
                    div(
                      style = "background-color: #f1f7ff; padding: 15px; border-radius: 8px; text-align: center;",
                      h4("Interactive Visualisations", style = "color: #0062cc; margin: 0; font-size: 22px; font-weight: 600;"),
                      p("to analyze your proteins of interest", style = "margin: 5px 0 0 0; color: #555;")
                    )
                  ),
                  column(
                    width = 4,
                    div(
                      style = "background-color: #f1f7ff; padding: 15px; border-radius: 8px; text-align: center;",
                      h4("AI resource assistant", style = "color: #0062cc; margin: 0; font-size: 22px; font-weight: 600;"),
                      p("to help you perform custom analyses", style = "margin: 5px 0 0 0; color: #555;")
                    )
                  )
                )
              ),
              
              # Right side - Image with JavaScript
              column(
                width = 4, 
                # Keeping the JavaScript for interactivity 
                HTML("<div style='position: relative; cursor: pointer; width: 85%; margin: 0 auto; padding: 10;' ' 
                     onclick='enlargeImage(this)' 
                     onmouseover='showCaption(this)' 
                     onmouseout='hideCaption(this)'>
                       
                       <!-- Image (80% size) -->
                       <img src='static/abstract.jpg' alt='Graphical Abstract' 
                     style='width: 100%; display: block; border-radius: 4px;'>
                       
                       <!-- Caption only shown on hover (initially hidden) -->
                       <div class='img-caption' style='position: absolute; bottom: 0; left: 0; right: 0; 
                                 background-color: rgba(0,0,0,0.6); color: white; 
                                 padding: 15px; z-index: 2; opacity: 0; 
                                 transition: opacity 0.3s ease;'>
                       <h3 style='margin: 0; font-weight: 500;'>
                      Temporal Alterations in The Nascent Proteome in Response to Mitochondrial Stress
                     </h3>
                       <p style='margin: 5px 0 0 0; font-size: 0.9em;'>
                       Show High Resolution
                     </p>
                       </div>
                       </div>
                       
                       <!-- Modal for enlarged image -->
                       <div id='imageModal' style='display: none; position: fixed; z-index: 9999; 
                           left: 0; top: 0; width: 100%; height: 100%; 
                           background-color: rgba(0,0,0,0.9);'>
                       <div style='display: flex; justify-content: center; align-items: center; height: 100%;'>
                       <img id='enlargedImg' style='max-width: 90%; max-height: 90%; object-fit: contain;'>
                       <div style='position: absolute; top: 20px; right: 30px; color: #f1f1f1; 
              font-size: 40px; font-weight: bold; cursor: pointer;'
                     onclick='closeModal()'>×</div>
                       </div>
                       </div>
                       
                       <script>
                       function showCaption(element) {
                         element.querySelector('.img-caption').style.opacity = '1';
                       }
                     
                     function hideCaption(element) {
                       element.querySelector('.img-caption').style.opacity = '0';
                     }
                     
                     function enlargeImage(element) {
                       var modal = document.getElementById('imageModal');
                       var modalImg = document.getElementById('enlargedImg');
                       var imgSrc = element.querySelector('img').src;
                       
                       modal.style.display = 'block';
                       modalImg.src = imgSrc;
                       
                       // Prevent scrolling when modal is open
                       document.body.style.overflow = 'hidden';
                     }
                     
                     function closeModal() {
                       document.getElementById('imageModal').style.display = 'none';
                       document.body.style.overflow = 'auto';
                     }
                     
                     // Close modal when clicking outside the image
                     document.getElementById('imageModal').addEventListener('click', function(event) {
                       if (event.target === this) {
                         closeModal();
                       }
                     });
                     </script>")
              )
            )
          )
        )
      )
    ),
    
    br(),
    
   
    
    
    # Capabilities and Abstract section
    fluidRow(
      column(
        width = 5,
        card(
          style = "height: 100%; border-radius: 8px; box-shadow: 0 4px 6px rgba(0,0,0,0.1);",
          card_header(
            h3("How you can interact with this data resource", style = "margin: 0; font-weight: 500;"),
            style = "background-color: #f8f9fa; border-bottom: 1px solid #eaeaea;"
          ),
          card_body( style = "padding: 20px;",
            p("Using this interactive tool, researchers can:"),
            tags$ul(
              style = "padding-left: 20px;",
              tags$li("Explore all datasets via interactive tables with intuitive filtering and sorting"),
              tags$li("Visualize protein expression changes during temporal alterations of the nascent proteome (during translation attenuation and recovery of inhibited protein synthesis)"),
              tags$li("Compare nascent proteome alterations following mitochondrial stress or proteasome inhibition"),
              tags$li("Compare nascent proteome alterations following EEF1A1 silencing +-mitochondrial stress"),
              tags$li("Access meta-analysis data for dynamically regulated nascent proteins"),
              tags$li("Leverage AI assistance for customized data exploration")
            )
          )
        )
      ),
      
      column(
        width = 7,
        card(
          style = "height: 100%; border-radius: 8px; box-shadow: 0 4px 6px rgba(0,0,0,0.1);",
          card_header(
            h3("Summary of the study", style = "margin: 0; font-weight: 500;"),
            style = "background-color: #f8f9fa; border-bottom: 1px solid #eaeaea;"
          ),
          card_body(
            p(
              style = "line-height: 1.6; font-size: 1.05em; padding: 20px;",
              "Under stress, protein synthesis is attenuated to preserve energy and mitigate challenges to protein homeostasis. Here, we describe, with high temporal resolution, the dynamic landscape of changes in the abundance of proteins synthesized upon stress from transient mitochondrial inner membrane depolarization. This nascent proteome was altered when global translation was attenuated by stress and began to normalize as translation was recovering. This transition was associated with a transient desynchronization of cytosolic and mitochondrial translation and recovery of cytosolic and mitochondrial ribosomal proteins. Further, the elongation factor EEF1A1 was downregulated upon mitochondrial stress, and its silencing mimicked the stress-induced nascent proteome remodeling, including alterations in the nascent respiratory chain proteins. Unexpectedly, the stress-induced alterations in the nascent proteome were independent of physiological protein abundance and turnover. In summary, we provide insights into the physiological and pathological consequences of mitochondrial function and dysfunction."
            )
          )
        )
      )
    ),
    
    br(),
    
  # Dataset information with accordion
    card(
      style = "border-radius: 8px; box-shadow: 0 4px 6px rgba(0,0,0,0.1);",
      card_header(
        h3("Available Datasets", style = "margin: 0; font-weight: 500;"),
        style = "background-color: #f8f9fa; border-bottom: 1px solid #eaeaea;"
      ),
      card_body(
        p("This resource includes five comprehensive datasets that can be explored in the Data Browser tab:"),
        
        datasets_accordion()
      )
    ),
    
    br(),
    
  # Team info with simplified layout
  card(
    style = "border-radius: 8px; box-shadow: 0 4px 6px rgba(0,0,0,0.1);",
    card_header(
      h3("Authors", style = "margin: 0; font-weight: 500;"),
      style = "background-color: #f8f9fa; border-bottom: 1px solid #eaeaea;"
    ),
    card_body(
      # Simple CSS for equal heights
      tags$style(HTML("
      /* Set equal heights for all value boxes */
      .team-box .bslib-value-box {
        height: 100%;
        min-height: 280px;
      }
      
      /* Center text in coauthors box */
      #coauthors-box p {
        text-align: center;
        margin-bottom: 5px;
      }
      
      /* Logo styling */
      .imol-logo {
        width: 120px;
        margin: 12px auto 0 auto;
        display: block;
      }
    ")),
      
      div(
        class = "team-box",
        layout_column_wrap(
          width = "250px",
          style = "margin-top: 15px;",
          
          # First author
          value_box(
            title = "First author / developer of this app",
            value = "Tomasz Stępkowski, PhD",
            showcase = img(
              src = "static/Stepkowski.png",
              alt = "Tomasz Stępkowski",
              style = "width: 100%; border-radius: 50%;"
            ),
            p("Freelance R/Shiny developer, Life Science Data Analyst"),
            p("IMol - Polish Academy of Sciences (Alumnus)"),
            p(a("datviser@gmail.com", href = "mailto:datviser@gmail.com"))
          ),
          
          # Principal Investigator
          value_box(
            title = "Principal Investigator",
            value = "Prof. Agnieszka Chacinska, PhD",
            showcase = img(
              src = "static/Chacinska.png",
              alt = "Agnieszka Chacinska",
              style = "width: 100%; border-radius: 50%;"
            ),
            p("IMol - Polish Academy of Sciences"),
            p("Head of the Laboratory of Mitochondrial Biogenesis"),
            p(a("achacinska@imol.institute", href = "mailto:a.chacinska@imol.institute"))
          ),
          
          # Coauthors - simplified
          value_box(
            id = "coauthors-box",
            # Create a properly structured title with image and text side by side
             
            title = div(style = "font-weight: bold;",
                p("The study was conducted at"),
                p(span(img(
                  src = "static/Imol.png", 
                  alt = "IMol Logo",
                  style = "width: 80px; height: auto;"
                )),a("(Polish Academy of Sciences)", href = "https://imol.institute/")),
                br(),
                  p("The other coauthors of the study are:")
              ),
           
            # Remove showcase and value
            showcase = NULL,
            value = NULL,
            
            # Coauthor list
            p("Vanessa Linke, PhD (IMol Alumna, IIMCB)"),
            p("Dorota Stadnik, PhD (IMol Alumna)"),
            p("Maciej Zakrzewski, PhD (IMol)"),
            p("Anna Zawada, PhD (IMol Alumna, ICTER)"),
            p("Remigiusz Serwa, PhD (IMol)")
            
            
          )
        )
      )
    )
  ),
    
    # Footer with technical info
    card(
      style = "border-radius: 8px; box-shadow: 0 4px 6px rgba(0,0,0,0.1);",
      card_body(
        style = "padding: 15px;",
        div(
          style = "display: flex; justify-content: space-between; align-items: center;",
          div(
            p(
              style = "margin: 0;",
              "Built with ",
              a("R Shiny", href = "https://shiny.posit.co/", target = "_blank"),
              " and ",
              a("Rhino framework", href = "https://appsilon.github.io/rhino/", target = "_blank")
            )
          ),
          div(
                        p(
              style = "margin: 0; text-align: right;",
              "Developed by Tomasz Stępkowski",
              img(
                src = "static/Datviser.png", 
                alt = "DatViseR Logo",
                style = "height: 60px; width:auto, vertical-align: middle;"
              ),
              
              
              a(
                "Source Code", 
                href = "https://github.com/DatViseR/Cell_Reports_Stepkowski", 
                target = "_blank"
              )
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
    # Server logic for the About module
    # (minimal for a static About page)
  })
}