# This module draws the interactive volcano plot using plotly, It takes
#dataset$genes, dataset$FC, dataset$pval, dataset$qvalue and
# one or more annotations (GO category name, GO genes from this category and the the GO:ID),
#or custom genes as input and highlights them in the plot.

box::use(
  plotly[plotlyOutput, renderPlotly],

  shiny[
    moduleServer,
    NS,
    plotOutput,
    renderPlot,
    observeEvent,
    req,
    div,
    span,
    p,
    tags,
    reactiveVal,
    reactive,
    isolate,
  ],
  plotly[
    plot_ly,
    layout,
    event_data,
    add_markers,
    add_annotations,
    config,
    toRGB,
    highlight_key
  ],
  dplyr[filter, mutate, arrange, rename, select, left_join, case_when],
  htmlwidgets[onRender],
  stats[quantile],
  grDevices[colorRampPalette]
)

#' Create a volcano plot UI
#'
#' @param id Module ID
#' @param height Plot height (default: "600px")
#' @param width Plot width (default: "100%")
#'
#' @export
ui <- function(id, height = "600px", width = "100%") {
  ns <- NS(id)

  div(
    plotlyOutput(ns("volcano_plot"), height = height, width = width),
    div(
      style = "font-size: 90%; color: #666; margin-top: 10px;",
      p(
        span(
          tags$b("Hover"),
          "over points to see details. ",
          span(tags$b("Click"), "on points to select genes.")
        )
      )
    )
  )
}

#' Create a volcano plot server
#'
#' @param id Module ID
#' @param dataset Reactive expression containing a data frame with
#'   genes, FC (fold change), pval (p-value), and qvalue columns
#' @param highlight_genes Reactive expression containing gene names to highlight
#' @param highlight_info Optional reactive expression containing annotation info (e.g., GO:ID, GO name)
#' @param fc_cutoff Reactive expression containing fold change cutoff (default: 1)
#' @param pval_cutoff Reactive expression containing p-value cutoff (default: 0.05)
#' @param title Optional reactive expression containing plot title
#'
#' @return A list with reactive expressions for selected_genes and rendered_plot
#'
#' @export
server <- function(
  id,
  dataset,
  highlight_genes = reactive(NULL),
  highlight_info = reactive(NULL),
  fc_cutoff = reactive(1),
  pval_cutoff = reactive(0.05),
  title = reactive("Volcano Plot")
) {
  moduleServer(id, function(input, output, session) {
    # Process the data for plotting
    plot_data <- reactive({
      req(dataset())

      data <- dataset()

      # Ensure required columns exist
      required_cols <- c("genes", "FC", "pval")
      if (!all(required_cols %in% names(data))) {
        missing <- required_cols[!required_cols %in% names(data)]
        stop(
          "Missing required columns in dataset: ",
          paste(missing, collapse = ", ")
        )
      }

      # Create volcano plot data with significance categories
      data <- mutate(
        data,
        # Calculate negative log10 of p-value for y-axis
        neg_log_pval = -log10(pval),
        # Calculate log2 fold change for x-axis if not already in log scale
        log2FC = if ("log2FC" %in% names(data)) data$log2FC else log2(FC),
        # Determine point significance category
        significance = case_when(
          abs(log2FC) >= log2(fc_cutoff()) & pval < pval_cutoff() ~
            if (log2FC >= 0) "Upregulated" else "Downregulated",
          TRUE ~ "Not Significant"
        ),
        # Check if gene should be highlighted
        highlighted = genes %in% highlight_genes()
      )

      return(data)
    })

    # Create volcano plot
    output$volcano_plot <- renderPlotly({
      req(plot_data())

      data <- plot_data()

      # Create color scale for points
      colors <- c(
        "Upregulated" = "#FF4136", # Red
        "Downregulated" = "#0074D9", # Blue
        "Not Significant" = "#AAAAAA" # Grey
      )

      # Base plot
      p <- plot_ly(data = data, source = "volcano") %>%
        add_markers(
          x = ~log2FC,
          y = ~neg_log_pval,
          color = ~significance,
          colors = colors,
          symbol = ~highlighted,
          symbols = c("circle", "diamond"),
          size = ~ ifelse(highlighted, 10, 8),
          opacity = ~ ifelse(highlighted, 1, 0.7),
          text = ~ paste(
            "Gene:",
            genes,
            "<br>",
            "log2(FC):",
            round(log2FC, 3),
            "<br>",
            "p-value:",
            signif(pval, 3),
            if ("qvalue" %in% names(data)) {
              paste("<br>q-value:", signif(data$qvalue, 3))
            } else {
              ""
            }
          ),
          hoverinfo = "text"
        ) %>%
        layout(
          title = list(
            text = title(),
            font = list(size = 18)
          ),
          xaxis = list(
            title = "log2(Fold Change)",
            zeroline = TRUE,
            zerolinecolor = toRGB("black", 0.4),
            zerolinewidth = 1
          ),
          yaxis = list(
            title = "-log10(p-value)"
          ),
          legend = list(
            orientation = "h",
            y = -0.2
          ),
          shapes = list(
            # Vertical lines for fold change cutoff
            list(
              type = "line",
              x0 = log2(fc_cutoff()),
              x1 = log2(fc_cutoff()),
              y0 = 0,
              y1 = max(data$neg_log_pval, na.rm = TRUE),
              line = list(color = "grey", dash = "dash")
            ),
            list(
              type = "line",
              x0 = -log2(fc_cutoff()),
              x1 = -log2(fc_cutoff()),
              y0 = 0,
              y1 = max(data$neg_log_pval, na.rm = TRUE),
              line = list(color = "grey", dash = "dash")
            ),
            # Horizontal line for p-value cutoff
            list(
              type = "line",
              x0 = min(data$log2FC, na.rm = TRUE),
              x1 = max(data$log2FC, na.rm = TRUE),
              y0 = -log10(pval_cutoff()),
              y1 = -log10(pval_cutoff()),
              line = list(color = "grey", dash = "dash")
            )
          )
        ) %>%
        config(
          toImageButtonOptions = list(
            format = "png",
            filename = "volcano_plot",
            width = 1200,
            height = 800
          ),
          displaylogo = FALSE,
          modeBarButtonsToRemove = c(
            "sendDataToCloud",
            "editInChartStudio",
            "lasso2d",
            "autoScale2d"
          )
        )

      # Add annotation for highlighted genes if information is provided
      if (!is.null(highlight_info())) {
        info <- highlight_info()

        # Only add if there's actual highlighted content
        if (
          !is.null(info) &&
            !is.null(highlight_genes()) &&
            length(highlight_genes()) > 0
        ) {
          # Find a good position for annotation (upper right corner by default)
          x_pos <- quantile(data$log2FC, 0.85, na.rm = TRUE)
          y_pos <- quantile(data$neg_log_pval, 0.85, na.rm = TRUE)

          annotation_text <- paste0(
            "Highlighted: ",
            if ("name" %in% names(info)) info$name else "",
            if ("id" %in% names(info)) paste0(" (", info$id, ")") else "",
            "<br>",
            length(highlight_genes()),
            " genes"
          )

          p <- p %>%
            add_annotations(
              x = x_pos,
              y = y_pos,
              text = annotation_text,
              showarrow = FALSE,
              bgcolor = "rgba(255, 255, 255, 0.8)",
              bordercolor = "rgba(0, 0, 0, 0.2)",
              borderwidth = 1,
              font = list(size = 12)
            )
        }
      }

      return(p)
    })

    # Track selected points
    selected_genes <- reactive({
      # Get clickData from plotly
      event_data <- event_data("plotly_click", source = "volcano")

      if (is.null(event_data)) {
        return(NULL)
      }

      # Get the index of clicked point
      point_index <- event_data$pointNumber + 1 # plotly is 0-indexed

      # Return the gene at that index
      if (point_index <= nrow(plot_data())) {
        return(plot_data()$genes[point_index])
      }

      return(NULL)
    })

    # Return reactive values for external use
    return(list(
      selected_genes = selected_genes,
      rendered_plot = reactive(output$volcano_plot)
    ))
  })
}
