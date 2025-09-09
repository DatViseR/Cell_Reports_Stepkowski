# Generic Volcano module
# - Works with different dataset structures without requiring data transformation
# - Fixed x range:  -3 to 3
# - Fixed y range:   0 to 2.5
# - Visually square via CSS aspect-ratio
# - Threshold lines clipped to plotting window
# - Flexible column mapping for different dataset formats

box::use(
  shiny[moduleServer, NS, reactive, req, validate, need],
  plotly[
    plot_ly,
    add_markers,
    add_text,
    layout,
    config,
    event_data
  ],
  dplyr[mutate, case_when, arrange, desc, slice_head]
)

# UI: square container using aspect-ratio. height argument kept for backward compatibility
#' @export
ui <- function(id, height = "100%", width = "100%") {
  ns <- NS(id)
  shiny::div(
    # Wrapper enforces square shape
    shiny::div(
      style = paste(
        "position: relative;",
        "width:",
        width,
        ";",
        "aspect-ratio: 1 / 1;", # keeps it square responsively
        "max-width: 100%;"
      ),
      plotly::plotlyOutput(
        ns("volcano_plot"),
        height = "100%", # fill wrapper
        width = "100%"
      )
    )
  )
}

#' @export
server <- function(
  id,
  dataset,
  log2fc_column,
  pvalue_column = NULL,
  qvalue_column = NULL,
  gene_column,
  go_annotations = reactive(NULL),
  custom_highlights = reactive(NULL),
  fc_cutoff = reactive(1.5),
  q_cutoff = reactive(0.05),
  title = reactive("Volcano"),
  max_custom_labels = reactive(30),
  label_font_size = reactive(11),
  X_MIN = reactive(-3),
  X_MAX = reactive(-3),
  Y_MIN = reactive(0),
  Y_MAX = reactive(2.5)
) {
  moduleServer(id, function(input, output, session) {
    prepared <- reactive({
      req(dataset())
      df <- dataset()

      # Validate required columns exist
      validate(
        need(
          log2fc_column %in% names(df),
          paste("Column", log2fc_column, "missing.")
        ),
        need(
          gene_column %in% names(df),
          paste("Column", gene_column, "missing.")
        ),
        need(
          !is.null(pvalue_column) || !is.null(qvalue_column),
          "Either pvalue_column or qvalue_column must be specified."
        )
      )

      if (!is.null(qvalue_column)) {
        validate(need(
          qvalue_column %in% names(df),
          paste("Column", qvalue_column, "missing.")
        ))
        y_col <- qvalue_column
        metric_label <- "q-value"
        using_q <- TRUE
      } else {
        validate(need(
          pvalue_column %in% names(df),
          paste("Column", pvalue_column, "missing.")
        ))
        y_col <- pvalue_column
        metric_label <- "p-value"
        using_q <- FALSE
      }

      # Remove rows with missing critical values
      sub <- df[
        !is.na(df[[log2fc_column]]) &
          !is.na(df[[y_col]]) &
          !is.na(df[[gene_column]])
      ]

      if (!nrow(sub)) {
        return(sub)
      }

      # Handle p-values/q-values <= 0 or > 1
      sub[[y_col]][is.na(sub[[y_col]]) | sub[[y_col]] <= 0] <-
        min(sub[[y_col]][sub[[y_col]] > 0], na.rm = TRUE)
      sub[[y_col]][sub[[y_col]] > 1] <- 1

      # Create standardized column names for plotting
      sub$genes <- sub[[gene_column]]
      sub$log2FC <- sub[[log2fc_column]]
      sub$metric <- sub[[y_col]]
      sub$neg_log_metric <- -log10(sub$metric)

      fc_thr <- log2(fc_cutoff())
      q_thr <- q_cutoff()

      sub <- mutate(
        sub,
        significance = case_when(
          abs(log2FC) >= fc_thr & metric < q_thr & log2FC > 0 ~ "Up",
          abs(log2FC) >= fc_thr & metric < q_thr & log2FC <= 0 ~ "Down",
          TRUE ~ "NotSig"
        ),
        metric_label = metric_label,
        using_q = using_q
      )
      sub
    })

    output$volcano_plot <- plotly::renderPlotly({
      dat <- prepared()
      if (is.null(dat) || !nrow(dat)) {
        return(
          plotly::plot_ly() |>
            layout(
              annotations = list(
                text = "No data available",
                x = 0.5,
                y = 0.5,
                xref = "paper",
                yref = "paper",
                showarrow = FALSE
              )
            )
        )
      }

      # Fixed ranges
      X_MIN <- -3
      X_MAX <- 3
      Y_MIN <- 0
      Y_MAX <- 2.5

      fc_line <- log2(fc_cutoff())
      q_line_y <- -log10(q_cutoff())

      draw_q_line <- (q_line_y >= Y_MIN && q_line_y <= Y_MAX)

      base_cols <- list(
        NotSig = "#C7C7C7",
        Up = "#E74C3C",
        Down = "#1F77B4"
      )

      ns <- dat[dat$significance == "NotSig", , drop = FALSE]
      up <- dat[dat$significance == "Up", , drop = FALSE]
      dn <- dat[dat$significance == "Down", , drop = FALSE]

      p <- plot_ly(
        data = ns,
        x = ~log2FC,
        y = ~neg_log_metric,
        type = "scatter",
        mode = "markers",
        marker = list(color = base_cols$NotSig, size = 5, opacity = 0.55),
        text = ~ paste0(
          "<b>",
          genes,
          "</b><br>",
          "log2(FC): ",
          round(log2FC, 3),
          "<br>",
          "-log10(",
          metric_label,
          "): ",
          round(neg_log_metric, 3),
          "<br>",
          metric_label,
          ": ",
          signif(metric, 3)
        ),
        hoverinfo = "text",
        customdata = ~genes,
        name = "Not significant",
        legendgroup = "Significance",
        showlegend = FALSE,
        source = "volcano"
      )

      if (nrow(dn)) {
        p <- p |>
          add_markers(
            data = dn,
            x = ~log2FC,
            y = ~neg_log_metric,
            marker = list(color = base_cols$Down, size = 6),
            text = ~ paste0(
              "<b>",
              genes,
              "</b><br>",
              "Downregulated<br>",
              "log2(FC): ",
              round(log2FC, 3),
              "<br>",
              "-log10(",
              metric_label,
              "): ",
              round(neg_log_metric, 3),
              "<br>",
              metric_label,
              ": ",
              signif(metric, 3)
            ),
            hoverinfo = "text",
            customdata = ~genes,
            name = "Downregulated",
            legendgroup = "Significance",
            showlegend = FALSE
          )
      }

      if (nrow(up)) {
        p <- p |>
          add_markers(
            data = up,
            x = ~log2FC,
            y = ~neg_log_metric,
            marker = list(color = base_cols$Up, size = 6),
            text = ~ paste0(
              "<b>",
              genes,
              "</b><br>",
              "Upregulated<br>",
              "log2(FC): ",
              round(log2FC, 3),
              "<br>",
              "-log10(",
              metric_label,
              "): ",
              round(neg_log_metric, 3),
              "<br>",
              metric_label,
              ": ",
              signif(metric, 3)
            ),
            hoverinfo = "text",
            customdata = ~genes,
            name = "Upregulated",
            legendgroup = "Significance",
            showlegend = FALSE
          )
      }

      # GO annotations
      go_list <- go_annotations()
      if (!is.null(go_list) && length(go_list)) {
        available_genes <- unique(dat$genes)
        for (gl in go_list) {
          if (
            is.null(gl) ||
              any(is.null(c(gl$category, gl$genes, gl$color)))
          ) {
            next
          }
          sub_go <- dat[dat$genes %in% gl$genes, , drop = FALSE]
          if (!nrow(sub_go)) {
            next
          }
          p <- p |>
            add_markers(
              data = sub_go,
              x = ~log2FC,
              y = ~neg_log_metric,
              marker = list(
                color = gl$color,
                size = 9,
                line = list(color = "#222222", width = 0.7),
                symbol = "circle"
              ),
              text = ~ paste0(
                "<b>",
                genes,
                "</b><br>",
                "GO: ",
                gl$category,
                "<br>",
                "log2(FC): ",
                round(log2FC, 3),
                "<br>",
                "-log10(",
                metric_label,
                "): ",
                round(neg_log_metric, 3),
                "<br>",
                metric_label,
                ": ",
                signif(metric, 3)
              ),
              hoverinfo = "text",
              customdata = ~genes,
              name = gl$category,
              legendgroup = "GO",
              showlegend = TRUE
            )
        }
      }

      # Custom genes + labels
      custom_vec <- custom_highlights()
      if (!is.null(custom_vec) && length(custom_vec)) {
        sub_c <- dat[dat$genes %in% custom_vec, , drop = FALSE]
        if (nrow(sub_c)) {
          max_labels <- max_custom_labels()
          sub_label <- if (nrow(sub_c) > max_labels) {
            sub_c |>
              arrange(desc(neg_log_metric)) |>
              slice_head(n = max_labels)
          } else {
            sub_c
          }

          p <- p |>
            add_markers(
              data = sub_c,
              x = ~log2FC,
              y = ~neg_log_metric,
              marker = list(
                color = "#000000",
                size = 11,
                symbol = "diamond",
                line = list(color = "#FFFFFF", width = 1.4)
              ),
              text = ~ paste0(
                "<b>",
                genes,
                "</b><br>",
                "Custom selection<br>",
                "log2(FC): ",
                round(log2FC, 3),
                "<br>",
                "-log10(",
                metric_label,
                "): ",
                round(neg_log_metric, 3),
                "<br>",
                metric_label,
                ": ",
                signif(metric, 3)
              ),
              hoverinfo = "text",
              customdata = ~genes,
              name = "Custom genes",
              legendgroup = "Custom",
              showlegend = TRUE
            ) |>
            add_text(
              data = sub_label,
              x = ~log2FC,
              y = ~neg_log_metric,
              text = ~genes,
              textposition = "top center",
              textfont = list(color = "#000000", size = label_font_size()),
              hoverinfo = "none",
              showlegend = FALSE
            )
        }
      }

      # Threshold shapes
      shapes <- list(
        list(
          type = "line",
          x0 = fc_line,
          x1 = fc_line,
          y0 = Y_MIN,
          y1 = Y_MAX,
          line = list(color = "gray50", dash = "dash", width = 1)
        ),
        list(
          type = "line",
          x0 = -fc_line,
          x1 = -fc_line,
          y0 = Y_MIN,
          y1 = Y_MAX,
          line = list(color = "gray50", dash = "dash", width = 1)
        )
      )
      if (draw_q_line) {
        shapes[[length(shapes) + 1]] <- list(
          type = "line",
          x0 = X_MIN,
          x1 = X_MAX,
          y0 = q_line_y,
          y1 = q_line_y,
          line = list(color = "gray50", dash = "dash", width = 1)
        )
      }

      metric_axis_label <- if (dat$using_q[1]) {
        "-log10(q-value)"
      } else {
        "-log10(p-value)"
      }

      p |>
        layout(
          title = NULL,
          xaxis = list(
            title = "log2(Fold Change)",
            range = c(X_MIN, X_MAX),
            zeroline = TRUE,
            zerolinecolor = "rgba(0,0,0,0.25)"
          ),
          yaxis = list(
            title = metric_axis_label,
            range = c(Y_MIN, Y_MAX)
            # No scaleanchor => visually square container takes over
          ),
          legend = list(orientation = "h", y = -0.25),
          shapes = shapes
        ) |>
        config(
          displaylogo = FALSE,
          responsive = TRUE,
          modeBarButtonsToRemove = c(
            "lasso2d",
            "select2d",
            "zoomIn2d",
            "zoomOut2d",
            "autoScale2d"
          ),
          toImageButtonOptions = list(
            format = "png",
            filename = paste0("volcano_", gsub("[^A-Za-z0-9_]", "_", title())),
            width = 1200,
            height = 1200
          )
        )
    })

    selected_genes <- reactive({
      ev <- event_data("plotly_click", source = "volcano")
      if (is.null(ev) || is.null(ev$customdata)) {
        return(NULL)
      }
      ev$customdata
    })

    return(list(selected_genes = selected_genes))
  })
}
