box::use(
  shiny[moduleServer, NS, div, h3, br, req],
  highcharter[highchart, hc_add_series, hc_chart, hc_xAxis, hc_yAxis, hc_title,
              hc_tooltip, highchartOutput, renderHighchart],
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
      data <- read_csv("data/Dataset_5_cleaned.csv")
      
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
        hc_xAxis(title = list(text = "mean log2(fold)")) |>
        hc_yAxis(title = list(text = ""), max = 50) |>
        hc_tooltip(
          headerFormat = "",
          pointFormat = "<b>{point.gene}</b><br>Fold: {point.fold:.2f}"
        )
      
      # Add the dots one by one directly
      for (i in 1:nrow(binned_data)) {
        if (binned_data$stack_position[i] <= 50) {  # Limit to 50 high
          hc <- hc |> hc_add_series(
            data = list(list(
              x = binned_data$bin_center[i],
              y = binned_data$stack_position[i],
              gene = binned_data$Gene_single[i],
              fold = binned_data$`mean log2(fold)`[i]
            )),
            type = "scatter",
            name = binned_data$Gene_single[i],
            showInLegend = FALSE,
            color = ifelse(binned_data$`mean log2(fold)`[i] > 0, "#E74C3C", "#3498DB"),
            marker = list(
              symbol = "circle",
              radius = 5
            )
          )
        }
      }
      
      return(hc)
    })
  })
}