box::use(
  shiny[moduleServer, NS, div, h3, br, req],
  highcharter[highchart, hc_add_series, hc_chart, hc_xAxis, hc_yAxis, hc_title,
              hc_tooltip, highchartOutput, renderHighchart, hc_plotOptions],
  dplyr[mutate, group_by, summarize, n],
  readr[read_csv]
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  div(
    class = "Meta_analysis-container",
    style = "padding: 20px;",
    
    h3("Gene Expression Dotplot", style = "color: #0062cc;"),
    br(),
    
    highchartOutput(ns("gene_dotplot"), height = "600px")
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
        hc_title(text = "Gene Expression Dotplot") |>
        hc_xAxis(
          title = list(text = "mean log2(fold)"),
          plotBands = list(list(
            from = -0.25,
            to = 0.25,
            color = "rgba(200, 200, 200, 0.2)"
          ))
        ) |>
        hc_yAxis(title = list(text = ""), max = 50) |>
        hc_tooltip(
          headerFormat = "",
          pointFormat = "<b>{point.gene}</b><br>Fold: {point.fold:.2f}<br>P-value: {point.pvalue:.2f}"
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
              pvalue = binned_data$`mean -log10  p value`[i]
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