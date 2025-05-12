box::use(
  shiny[moduleServer, NS, div, h3,h4,h5, br, req, imageOutput, renderImage, img, tabsetPanel, tabPanel, p, column, fluidRow, tags],
  highcharter[highchart, hc_add_series, hc_chart, hc_xAxis, hc_yAxis, hc_title,
              hc_tooltip, highchartOutput, renderHighchart, hc_plotOptions, JS],  # Added JS directly from highcharter
  dplyr[mutate, group_by, summarize, n],
  readr[read_csv],
  here[here]
    
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  
  div(
    class = "Meta_analysis-container",
    style = "padding: 20px;",
    
    h3("Dataset V: Meta-analysis of CCCP-regulated nascent proteome", style = "color: #0062cc; margin-bottom: 20px;"),
    
    tabsetPanel(
      id = ns("meta_analysis_tabs"),
      tabPanel(
        title = "High Confidence CCCP Regulated Proteome",
        value = "chart_tab",
        div(
          style = "padding: 15px 0;",
          p("This is the interactive visualisation of the high confidence CCCP regulated nascent proteoem. It shows distribution of mean log2 fold changes CCCP vs. DMSO for proteins regulated by CCCP across multiple datasets.
          Click on the protein symbol to see detailed information including protein function and mean expression changes and assosiated p-values"),
          highchartOutput(ns("gene_dotplot"), height = "600px")
        )
      ),
      tabPanel(
        title = "About the Meta-Analysis",
        value = "about_tab",
       
          
          fluidRow(
            # Left column - Image
            column(
              width = 5,
              div(
                style = "padding-right: 15px;",
                img(
                  src = "static/meta-analysis.png", 
                  alt = "Meta-analysis methodology", 
                  style = "width: 100%; border: 1px solid #ddd; border-radius: 5px;"
                ),
                # add figure caption
                 h5("Figure 1: Meta-analysis methodology", style = "text-align: center; color: #666;")
                  )
            ),
            
            # Right column - Description
            column(
              width = 7,
              div(
                style = "padding-left: 15px;",
                h4("Meta-analysis inclusion criteria", style = "color: #0062cc; margin-top: 0;"),
                tags$ul(
                  style = "list-style-type: disc; padding-left: 20px;",
                  tags$li("The significantly regulated proteins were extracted from each dataset"),
                  tags$li("Hits were filtered for linear absolute fold expresssion difference > 20%"),
                  tags$li("Only hits significantly regulated in at least 3/4 datasets were included"),
                ),
                h4("Top 25 hits in the meta-analysis", style = "color: #0062cc; margin-top: 20px;"),
                img(
                  src = "static/meta-analysis_tab.png", 
                  alt = "Meta-analysis tab", 
                  style = "width: 41%; border: 1px solid #ddd; border-radius: 5px;"
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
    
  
    
    output$gene_dotplot <- renderHighchart({
      # Load data
      data <- read_csv("data/Dataset_5_cleaned.csv", show_col_types = FALSE)
      
      # Simple approach: manually bin the data
      bin_width <- 0.1
      data$bin_center <- round(data$`mean log2(fold)` / bin_width) * bin_width
      
      # Count genes in each bin for stacking
      binned_data <- data |>
        group_by(bin_center) |>
        mutate(stack_position = 1:n())
      
      # Create the basic highchart
      hc <- highchart() |>
        hc_chart(type = "scatter") |>
        hc_title(text = "Nascent proteins significantly regulated by CCCP in at least 3/4 datasets") |>
        hc_xAxis(
          title = list(text = "mean log2(fold)"),
          plotBands = list(list(
            from = -0.25,
            to = 0.25,
            color = "white"
          ),
          list(
            from = 0.25,
            to = 1.8,
            color = "rgba(200, 200, 200, 0.2)"
          ),
          list(
            from = -1.8,
            to = -0.25,
            color = "rgba(200, 200, 200, 0.2)"
          ))) |>
        hc_yAxis(title = list(text = ""), max = 50) |>
        hc_tooltip(
          useHTML = TRUE,
          headerFormat = "",
          pointFormat = '<div style="width: 438px; max-width: 100%; padding: 10px; font-family: Arial, sans-serif;">
                           <h3 style="margin: 0 0 10px 0; padding-bottom: 5px; border-bottom: 1px solid #ddd; color: #0062cc;">
                             <span style="color: {point.colorValue};">{point.gene}</span>
                           </h3>
                           <table style="width: 100%; border-collapse: collapse;">
                             <tr>
                               <td style="padding: 3px; font-weight: bold;">Fold Change:</td>
                               <td style="padding: 3px;">{point.fold:.2f}</td>
                             </tr>
                             <tr>
                               <td style="padding: 3px; font-weight: bold;">P-value:</td>
                               <td style="padding: 3px;">{point.pvalue:.2f}</td>
                             </tr>
                             <tr>
                               <td style="padding: 3px; font-weight: bold;">Significant in:</td>
                               <td style="padding: 3px;">{point.significant}/4 datasets</td>
                             </tr>
                           </table>
                           <div style="margin-top: 8px; width: 418px;">
                             <div style="font-weight: bold; margin-bottom: 3px;">Protein:</div>
                             <div style="padding-left: 5px; font-size: 12px; white-space: normal; word-break: break-word; overflow-wrap: break-word;">{point.protein_names}</div>
                           </div>
                           <div style="margin-top: 8px; width: 418px;">
                             <div style="font-weight: bold; margin-bottom: 3px;">Function:</div>
                             <div style="padding-left: 5px; font-size: 12px; white-space: normal; word-break: break-word; overflow-wrap: break-word;">{point.function_cc}</div>
                           </div>
                         </div>',
          backgroundColor = "rgba(255, 255, 255, 0.98)",
          borderWidth = 1,
          borderColor = "#AAA",
          borderRadius = 8,
          shadow = TRUE,
          outside = TRUE,
          followPointer = FALSE
        ) |>
        hc_plotOptions(
          scatter = list(
            dataLabels = list(
              enabled = TRUE,
              format = "{point.gene}"
            )
          )
        )
      
      # Add the dots one by one directly, but show gene names instead
      for (i in 1:nrow(binned_data)) {
        # Skip points in the break range
        if (binned_data$bin_center[i] > -0.25 && binned_data$bin_center[i] < 0.25) {
          # Adjust bin centers to account for the break
          # Points near 0.25 will be shifted right, points near -0.25 shifted left
          if (binned_data$bin_center[i] >= 0) {
            binned_data$bin_center[i] <- 0.25
          } else {
            binned_data$bin_center[i] <- -0.25
          }
        }
        
        if (binned_data$stack_position[i] <= 50) {  # Limit to 50 high
          # Determine text color based on fold change
          text_color <- ifelse(binned_data$`mean log2(fold)`[i] > 0, "#E74C3C", "#3498DB")
          
          # Make text bolder for significant genes
          font_weight <- ifelse(binned_data$`significant in n/4 datasets`[i] == 4, "bold", "normal")
          
          hc <- hc |> hc_add_series(
            data = list(list(
              x = binned_data$bin_center[i],
              y = binned_data$stack_position[i],
              gene = binned_data$Gene_single[i],
              fold = binned_data$`mean log2(fold)`[i],
              pvalue = binned_data$`mean -log10  p value`[i],
              significant = binned_data$`significant in n/4 datasets`[i],
              function_cc = binned_data$`Function [CC]`[i],
              protein_names = binned_data$`Protein names`[i],
              colorValue = text_color
            )),
            type = "scatter",
            name = binned_data$Gene_single[i],
            showInLegend = FALSE,
            marker = list(
              enabled = FALSE  # Hide the marker
            ),
            dataLabels = list(
              enabled = TRUE,
              format = "{point.gene}",
              style = list(
                fontSize = "10px",
                fontWeight = font_weight,
                color = text_color
              )
            )
          )
        }
      }
      
      return(hc)
    })
  })
}