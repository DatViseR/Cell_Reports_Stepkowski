box::use(
  shiny[div, h5, p, HTML, tags, tagList],
  bslib[accordion, accordion_panel]
)

#' Create a formatted section for dataset information
#' 
#' @param title The title of the section
#' @param content The content text or HTML
#' @param color The border color for styling
#' @param icon Optional icon to display (HTML)
#' 
#' @return A shiny div with formatted content
#' @export
create_info_section <- function(title, content, color, icon = NULL) {
  div(
    style = paste0("background-color: #f8f9fa; border-left: 4px solid ", color, 
                   "; padding: 12px; border-radius: 0 8px 8px 0; height: 100%; font-size: 0.9rem;"),
    div(
      style = "display: flex; align-items: center; gap: 8px;",
      h5(title, style = paste0("margin-top: 0; margin-bottom: 8px; color: ", color, "; font-weight: 600; font-size: 0.95rem;")),
      if (!is.null(icon)) HTML(icon) else NULL
    ),
    # Handle content differently based on type
    if (is.character(content)) {
      # If it's a simple string, wrap in p()
      p(content, style = "margin-bottom: 0;")
    } else {
      # Otherwise, assume it's already HTML or UI elements
      div(style = "font-size: 0.9rem;", content)
    }
  )
}

#' Create a formatted dataset panel for the accordion
#' 
#' @param dataset_title Title of the dataset panel
#' @param method Method description
#' @param experimental_design Description of the experimental design
#' @param samples Sample information text
#' @param details Main details about the dataset
#' @param details_extra Optional additional details (default: NULL)
#' @param figures Corresponding figures in the manuscript
#' 
#' @return A formatted accordion panel
#' @export
create_dataset_panel <- function(
    dataset_title,
    method,
    experimental_design,
    samples,
    details,
    details_extra = NULL,
    figures
) {
  accordion_panel(
    dataset_title,
    div(
      style = "font-size: 0.9rem;",
      
      # Responsive grid layout - all sections in one row
      div(
        style = "display: grid; grid-template-columns: repeat(auto-fit, minmax(200px, 1fr)); gap: 10px; margin-top: 10px;",
        
        # Method section
        create_info_section(
          "Method:", 
          method, 
          "#0062cc", 
          icon = '<i class="fas fa-flask" style="color: #0062cc;"></i>'
        ),
        
        # Experimental Design section
        create_info_section(
          "Experimental Design:", 
          experimental_design, 
          "#6c757d",
          icon = '<i class="fas fa-project-diagram" style="color: #6c757d;"></i>'
        ),
        
        # Samples section
        create_info_section(
          "Samples:", 
          samples, 
          "#28a745",
          icon = '<i class="fas fa-vial" style="color: #28a745;"></i>'
        ),
        
        # Figures section
        create_info_section(
          "Corresponding Figures:", 
          figures, 
          "#0062cc",
          icon = '<i class="fas fa-chart-line" style="color: #0062cc;"></i>'
        ),
        
        # Details section - now in the same row
        create_info_section(
          "Details:", 
          tagList(
            p(details, style = "margin-bottom: 8px;"),
            if (!is.null(details_extra)) p(details_extra, style = "margin-bottom: 0;") else NULL
          ),
          "#333",
          icon = '<i class="fas fa-info-circle" style="color: #333;"></i>'
        )
      )
    )
  )
}

#' Create a complete dataset accordion with all panels
#' 
#' @return A complete accordion with all dataset panels
#' @export
datasets_accordion <- function() {
  # Add CSS for hover effects and font sizing
  hover_css <- tags$style(HTML("
    /* Add gentle hover effect to accordion headers */
    .accordion-button {
      transition: all 0.3s ease-in-out !important;
      font-size: 0.95rem !important;
    }
    
    .accordion-button:hover {
      background-color: #f0f7ff !important;
      color: #0056b3 !important;
      transform: translateX(3px) !important;
      box-shadow: -3px 0px 0px 0px #0062cc !important;
    }
    
    /* Add subtle indicator that items are interactive */
    .accordion-button:before {
      content: '';
      position: absolute;
      left: 0;
      height: 100%;
      width: 3px;
      background-color: transparent;
      transition: background-color 0.3s ease;
    }
    
    .accordion-button:hover:before {
      background-color: #0062cc;
    }
    
    /* Change cursor to pointer */
    .accordion-button {
      cursor: pointer;
    }
    
    /* Bullet points styling */
    .accordion-body ul {
      padding-left: 18px;
      margin-bottom: 0;
    }
    
    .accordion-body li {
      margin-bottom: 3px;
      font-size: 0.85rem;
    }
    
    /* Make all paragraphs in accordion smaller */
    .accordion-body p {
      font-size: 0.85rem;
      margin-bottom: 3px;
    }
  "))
  
  # Return the styled accordion
  tagList(
    hover_css,
    accordion(
      # Dataset I
      create_dataset_panel(
        dataset_title = "Dataset I: BONCAT - CCCP Stress and Recovery Time Course",
        method = tags$ul(
          tags$li("BONCAT LC-MS/MS"),
          tags$li("30 min AHA pulse labeling"),
          tags$li("TMT multiplexed proteomics")
        ),
        experimental_design = "Time course - 4 30min intervals - nascent protein labeling during treatment and wash from CCCP (translation inhibition and recovery)", 
        samples = "n = 32; replicates = 3; time points (0-30 (STRESS I), 30-60min (STRES II), 60-90min (RECOVERY I), 90-120 min (RECOVERY II))",
        details = "Translation is rapidly attenuated and rapidly recovers during first 30 min interval of wash, proteins related to ribosome, translation, OXPHOS and splicing are the most dynamically regulated",
        details_extra = "This is the preprocessed normalized data (translation attenuation was assesed on raw intensities - check the article)",
        figures = "Related to figures 1,3,4,5,7 in the Cell Reports article"
      ),
      
      # Dataset II
      create_dataset_panel(
        dataset_title = "Dataset II: CCCP or Bortezomib vs. DMSO",
        method = tags$ul(
          tags$li("BONCAT LC-MS/MS"),
          tags$li("1 hour AHA pulse labeling"),
          tags$li("TMT multiplexed proteomics")
        ),
        experimental_design = " 2.5h pretreatment with 20nm Bortezomib/DMSO
followed by 1h labeling with 2mM AHA together with 20nM Bortezomib or 8uM CCCP or DMSO",
        samples = "Replicates n = 4, samples ",
        details = "To estimate how strong the impact of protein degradation is on the nascent proteome and translation, 
        we treated the cells with the proteasome inhibitor bortezomib, which increased the abundance of ubiquitinated proteins 
        but in our conditions did not induce the rapid inhibition of cytosolic translation",
        details_extra = "Pronouced downregulation or ribosomal proteins is visible post CCCP treatment but not post bortezomib treatment.",
        figures = "Related to figure 2 in the manuscript"
      ),
      
      # Dataset III
      create_dataset_panel(
        dataset_title = "Dataset III: EEF1A1 Silencing Impact on Translation Dynamics",
        method = tags$ul(
          tags$li("siRNA silencing"),
          tags$li("BONCAT proteomics"),
          tags$li("30 min AHA pulse labeling")
        ),
        experimental_design = "Examination of translation dynamics after EEF1A1 silencing",
        samples = "n = 4 (Control siRNA, EEF1A1 siRNA replicates)",
        details = "Investigation of how EEF1A1 silencing affects the nascent proteome composition, revealing its role in the stress response pathway.",
        details_extra = "The dataset provides insights into translational regulation mechanisms during mitochondrial stress.",
        figures = "Figures 4A-D, S3A-B in the manuscript"
      ),
      
      # Dataset IV
      create_dataset_panel(
        dataset_title = "Dataset IV: HCMV Infection and Mitochondrial Stress Proteomics",
        method = tags$ul(
          tags$li("Viral infection model"),
          tags$li("BONCAT proteomics"),
          tags$li("30 min AHA pulse labeling")
        ),
        experimental_design = "Analysis of HCMV infection impact on nascent proteome under mitochondrial stress",
        samples = "n = 6 (Control, HCMV infected, with/without CCCP treatment)",
        details = "This dataset explores how viral infection (HCMV) interacts with mitochondrial stress response pathways to affect protein synthesis.",
        details_extra = "The comparative analysis provides insights into pathogen-induced stress responses.",
        figures = "Figures 5A-C, S4A-D in the manuscript"
      ),
      
      # Dataset V
      create_dataset_panel(
        dataset_title = "Dataset V: Meta-Analysis of Dynamically Regulated Nascent Proteins",
        method = tags$ul(
          tags$li("Integrated meta-analysis"),
          tags$li("Multi-dataset comparison"),
          tags$li("Merged proteomics data")
        ),
        experimental_design = "Cross-dataset analysis of consistently regulated proteins across stress conditions",
        samples = "n = All datasets combined (>2000 proteins analyzed)",
        details = "Comprehensive meta-analysis identifying proteins consistently regulated across all stress conditions.",
        details_extra = "This dataset highlights key proteins and pathways central to the cellular stress response, providing a systems-level view of mitochondrial stress adaptation.",
        figures = "Figures 6A-D, S5A-C in the manuscript"
      )
    )
  )
}