box::use(
  shiny[div, h4, p, HTML, tags, tagList]
)

#' Create a static dataset description UI component
#' 
#' @param dataset_id The dataset ID (I, II, III, IV, or V)
#' @return UI elements for dataset description
#' @export
create_description <- function(dataset_id) {
  # Dataset information from DataSet_Accordion.R
  dataset_info <- switch(dataset_id,
                         "I" = list(
                           title = "Dataset I: BONCAT - CCCP Stress and Recovery Time Course",
                           method = tagList(
                             tags$li("BONCAT LC-MS/MS"),
                             tags$li("30 min AHA pulse labeling"),
                             tags$li("TMT multiplexed proteomics")
                           ),
                           experimental_design = "Time course - 4 30min intervals - nascent protein labeling during treatment and wash from CCCP (translation inhibition and recovery)",
                           samples = "n = 32; replicates = 3; time points (0-30 (STRESS I), 30-60min (STRES II), 60-90min (RECOVERY I), 90-120 min (RECOVERY II))",
                           details = "Translation is rapidly attenuated and rapidly recovers during first 30 min interval of wash, proteins related to ribosome, translation, OXPHOS and splicing are the most dynamically regulated.",
                           details_extra = "This is the preprocessed normalized data (translation attenuation was assesed on raw intensities - check the article)",
                           figures = "Related to figures 1,3,4,5,7 in the Cell Reports article"
                         ),
                         "II" = list(
                           title = "Dataset II: CCCP or Bortezomib vs. DMSO",
                           method = tagList(
                             tags$li("BONCAT LC-MS/MS"),
                             tags$li("1 hour AHA pulse labeling"),
                             tags$li("TMT multiplexed proteomics")
                           ),
                           experimental_design = "2.5h pretreatment with 20nm Bortezomib/DMSO followed by 1h labeling with 2mM AHA together with 20nM Bortezomib or 8uM CCCP or DMSO",
                           samples = "Replicates n = 4, samples",
                           details = "To estimate how strong the impact of protein degradation is on the nascent proteome and translation, we treated the cells with the proteasome inhibitor bortezomib, which increased the abundance of ubiquitinated proteins but in our conditions did not induce the rapid inhibition of cytosolic translation",
                           details_extra = "Pronouced downregulation or ribosomal proteins is visible post CCCP treatment but not post bortezomib treatment.",
                           figures = "Related to figure 2 in the manuscript"
                         ),
                         "III" = list(
                           title = "Dataset III: Nascent proteome alterations after EEF1A1 silencing +- CCCP treatment",
                           method = tagList(
                             tags$li("siRNA silencing"),
                             tags$li("BONCAT proteomics"),
                             tags$li("1h AHA pulse labeling"),
                             tags$li("TMT multiplexed proteomics")
                           ),
                           experimental_design = "HEK293T were treated with siRNA scramble/siRNA EEF1A1 for 68h nascent proteome was labeled for 1h with 2mM AHA together with 8uM CCCP or 0.1% DMSO",
                           samples = "n = 3",
                           details = "Intrigued by our current and previous results showing a decrease in the EEF1A1 in conditions associated with mitochondrial stress, we silenced its expression and analyzed translation effects",
                           details_extra = "Silencing of EEF1A1 altered the nascent proteome in a pattern strikingly similar to that observed during acute mitochondrial stress",
                           figures = "Figure 6 in the manuscript"
                         ),
                         "IV" = list(
                           title = "Dataset IV: THRONCAT - nascent proteome changes during first and second 30 min of CCCP treatment",
                           method = tagList(
                             tags$li("THRONCAT (labeling in the presence of methionine"),
                             tags$li("Labeling in native growth media"),
                             tags$li("30 min betaES pulse labeling"),
                             tags$li("TMT multiplexed proteomics")
                           ),
                           experimental_design = "1mM concentration of betaES threonine analog was used to label nascent proteins in the first and second 30 min of CCCP treatment in HEK293T cells",
                           samples = "n = 4",
                           details = "Unavoidable limitation of BONCAT is the use of serum starvation during AHA labeling periods, which may impact stress signaling and regulation of the nascent proteome. To address this, we synthesized β-ethynylserine (βES) and applied the validation by the THRONCAT (dataset IV), which was reported to enable labeling of the nascent proteome in the full medium",
                           details_extra = "Nevertheless, due to the short time window, highly attenuated translation, and the presence of endogenous threonine in the medium, this validation is limited by the small number of quantified nascent proteins",
                           figures = "Only supplementary data available"
                         ),
                         "V" = list(
                           title = "Dataset V: Meta-Analysis of Dynamically Regulated Nascent Proteins",
                           method = tagList(
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
  
  # Return the formatted description UI with more compact layout
  div(
    style = "padding: 10px 0; font-size: 0.85rem;",
    
    h4(dataset_info$title, style = "color: #0062cc; font-weight: 600; margin-bottom: 12px; font-size: 1rem;"),
    
    # Using a 2-column grid layout for wider sidebar
    div(
      style = "display: grid; grid-template-columns: 1fr 1fr; gap: 10px; margin-bottom: 10px;",
      
      # Method section
      div(
        style = "background-color: #f8f9fa; border-left: 3px solid #0062cc; padding: 8px; border-radius: 0 6px 6px 0;",
        p(tags$b("Method:"), style = "margin: 0 0 3px 0; font-weight: 600; font-size: 0.8rem;"),
        tags$ul(style = "margin: 0; padding-left: 15px; font-size: 0.8rem;", dataset_info$method)
      ),
      
      # Experimental Design section
      div(
        style = "background-color: #f8f9fa; border-left: 3px solid #6c757d; padding: 8px; border-radius: 0 6px 6px 0;",
        p(tags$b("Design:"), style = "margin: 0 0 3px 0; font-weight: 600; font-size: 0.8rem;"),
        p(dataset_info$experimental_design, style = "margin: 0; font-size: 0.8rem;")
      )
    ),
    
    # Second row with 2 columns
    div(
      style = "display: grid; grid-template-columns: 1fr 1fr; gap: 10px; margin-bottom: 10px;",
      
      # Samples section
      div(
        style = "background-color: #f8f9fa; border-left: 3px solid #28a745; padding: 8px; border-radius: 0 6px 6px 0;",
        p(tags$b("Samples:"), style = "margin: 0 0 3px 0; font-weight: 600; font-size: 0.8rem;"),
        p(dataset_info$samples, style = "margin: 0; font-size: 0.8rem;")
      ),
      
      # Figures section
      div(
        style = "background-color: #f8f9fa; border-left: 3px solid #0062cc; padding: 8px; border-radius: 0 6px 6px 0;",
        p(tags$b("Figures:"), style = "margin: 0 0 3px 0; font-weight: 600; font-size: 0.8rem;"),
        p(dataset_info$figures, style = "margin: 0; font-size: 0.8rem;")
      )
    ),
    
    # Details section - full width
    div(
      style = "background-color: #f8f9fa; border-left: 3px solid #333; padding: 8px; border-radius: 0 6px 6px 0;",
      p(tags$b("Details:"), style = "margin: 0 0 3px 0; font-weight: 600; font-size: 0.8rem;"),
      p(dataset_info$details, style = "margin-bottom: 3px; font-size: 0.8rem;"),
      if (!is.null(dataset_info$details_extra)) p(dataset_info$details_extra, style = "margin: 0; font-size: 0.8rem;") else NULL
    ),
    
    # Divider and table note
    div(
      style = "margin-top: 12px; border-top: 1px dashed #ccc; padding-top: 8px;",
      p("Use the table below to explore the dataset.", style = "color: #6c757d; font-style: italic; font-size: 0.8rem; margin: 0;")
    )
  )
}