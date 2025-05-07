box::use(
  shiny[moduleServer, NS, div, h3, p, br, req, reactive],
  highcharter[highchart, hc_add_series, hc_chart, hc_xAxis, hc_yAxis, hc_title,
              hc_subtitle, hc_tooltip, hc_plotOptions, highchartOutput, 
              renderHighchart, hc_legend, hc_size],
  dplyr[mutate, filter, arrange, group_by, summarize, row_number],
  readr[read_csv],
  stats[density],
  grDevices[colorRampPalette]
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  div(
    class = "Meta_analysis-container",
    style = "padding: 20px;",
    
    h3("Meta-analysis", style = "color: #0062cc; margin-bottom: 20px;"),
    p("Explore the datasets from Stępkowski et al. Cell Reports 2024. The plot below shows gene expression data."),
    br(),
    
    highchartOutput(ns("gene_dotplot"), height = "600px"),
    br(),
    p("This dotplot histogram shows the distribution of mean log2(fold) values for each gene. 
      Red dots represent upregulated genes, blue dots represent downregulated genes.
      Size indicates significance: genes significant in 4/4 datasets are larger than those in 3/4.
      Hover over dots to see additional details about each gene.", 
      style = "font-style: italic; color: #666;")
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Load the data
    gene_data <- reactive({
      tryCatch({
        data <- read_csv("data/Dataset_5_cleaned.csv")
        # Normalize column names for easier access
        colnames(data)[1:5] <- c("Gene", "mean_log2fold", "mean_log10_pvalue", "significant_datasets", "rank")
        return(data)
      }, error = function(e) {
        message("Error loading data: ", e$message)
        NULL
      })
    })
    
    output$gene_dotplot <- renderHighchart({
      req(gene_data())
      
      data <- gene_data()
      
      # Extract bin midpoints for plotting (center of each bin)
      data$bin_mid <- NA
      
      # Extract bin midpoints using regex
      for (i in 1:nrow(data)) {
        bin_string <- as.character(data$bins[i])
        # Extract the two numbers from the bin range (e.g., "(0.3,0.4]")
        numbers <- as.numeric(unlist(regmatches(
          bin_string,
          gregexpr("-?[0-9]+(\\.[0-9]+)?", bin_string)
        )))
        
        if (length(numbers) == 2) {
          data$bin_mid[i] <- mean(numbers)
        }
      }
      
      # Count genes in each bin for stacking
      bin_counts <- table(data$bins)
      
      # Create position within each bin for stacking dots
      data <- data |>
        group_by(bins) |>
        mutate(position_in_bin = row_number())
      
      # Calculate max position in any bin for y-axis scaling
      max_bin_count <- max(bin_counts)
      
      # Create color palettes for red and blue gradients
      blue_palette <- colorRampPalette(c("#D6EAF8", "#2E86C1", "#1B4F72"))(50)  # Light to dark blue
      red_palette <- colorRampPalette(c("#F5B7B1", "#E74C3C", "#7B241C"))(50)   # Light to dark red
      
      # Create the highchart
      hc <- highchart() |>
        hc_chart(
          type = "scatter",
          zoomType = "xy",
          backgroundColor = "#FFFFFF"
        ) |>
        hc_title(text = "Gene Expression Dotplot Histogram") |>
        hc_subtitle(text = "Distribution of mean log2(fold) values") |>
        hc_xAxis(
          title = list(text = "mean log2(fold)"),
          gridLineWidth = 1
        ) |>
        hc_yAxis(
          title = list(text = "Count"),
          gridLineWidth = 1,
          max = max_bin_count * 1.3  # Leave room for density curve
        ) |>
        hc_legend(enabled = FALSE) |>
        hc_tooltip(
          useHTML = TRUE,
          headerFormat = "",
          pointFormat = paste0(
            "<b>Gene:</b> {point.gene}<br>",
            "<b>mean log2(fold):</b> {point.fold:.3f}<br>",
            "<b>-log10 p-value:</b> {point.pvalue:.3f}<br>",
            "<b>Significant in:</b> {point.significant}/4 datasets<br>",
            "<b>Rank:</b> {point.rank}"
          )
        )
      
      # Add density curve at the top
      dens <- density(data$mean_log2fold, n = 100)
      max_density <- max(dens$y)
      # Scale the density to place it above the dotplot
      density_scaling <- max_bin_count * 1.2  # Position density above the max bin count
      
      density_data <- lapply(1:length(dens$x), function(i) {
        list(x = dens$x[i], y = dens$y / max_density * density_scaling)
      })
      
      hc <- hc |> 
        hc_add_series(
          data = density_data,
          type = "spline",
          name = "Density",
          color = "rgba(50, 50, 50, 0.7)",
          lineWidth = 2,
          enableMouseTracking = FALSE
        )
      
      # Prepare point data
      point_data <- list()
      
      for (i in 1:nrow(data)) {
        gene <- data$Gene[i]
        fold <- data$mean_log2fold[i]
        bin_value <- data$bin_mid[i]
        pvalue <- data$mean_log10_pvalue[i]
        significant <- data$significant_datasets[i]
        rank <- data$rank[i]
        position_in_bin <- data$position_in_bin[i]
        
        # Calculate point size based on significance
        size <- ifelse(significant == 4, 8, 6)  # 4/4 datasets = larger dots
        
        # Determine color based on fold change
        color_idx <- NA
        if (fold > 0) {
          # Positive fold change: red scale
          intensity <- min(1, fold / max(data$mean_log2fold))
          color_idx <- round(intensity * (length(red_palette) - 1)) + 1
          color <- red_palette[color_idx]
        } else {
          # Negative fold change: blue scale
          intensity <- min(1, abs(fold) / abs(min(data$mean_log2fold)))
          color_idx <- round(intensity * (length(blue_palette) - 1)) + 1
          color <- blue_palette[color_idx]
        }
        
        point_data[[i]] <- list(
          x = bin_value,
          y = position_in_bin,  # This stacks dots vertically within each bin
          gene = gene,
          fold = fold,
          pvalue = pvalue,
          significant = significant,
          rank = rank,
          color = color,
          marker = list(
            radius = size,
            fillColor = color,
            lineWidth = 1,
            lineColor = "#000000"
          ),
          dataLabels = list(
            enabled = TRUE,
            format = gene,
            style = list(
              fontSize = "8px",
              fontWeight = "normal",
              textOutline = "1px contrast"
            )
          )
        )
      }
      
      # Add all points as a single series
      hc <- hc |> 
        hc_add_series(
          data = point_data,
          type = "scatter",
          name = "Genes",
          turboThreshold = 0  # Important for handling custom point attributes
        )
      
      return(hc)
    })
  })
}