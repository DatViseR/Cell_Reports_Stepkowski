# Volcano module (updated)
# - Parameter renamed: go_annotations (formerly go_highlights) to avoid name collisions.
# - Added square aspect ratio and adjustable height.
# - Uses q_value (if present) for significance; falls back to p_value if q_value missing.
# - Adds text labels for custom genes with a cap to prevent clutter.
# - Efficient single pass data prep; layered Plotly traces.
#
# Required columns in incoming dataset() (full long dataset):
#   Time_point, Gene_single or Gene_names, `log2(Fold)`, q_value (preferred) or p_value (fallback)
#
# Public API:
# ui(id, height, width)
# server(
#   id,
#   dataset,            # reactive: full long dataset
#   timepoint,          # scalar character (e.g. "STRESS_I")
#   go_annotations,     # reactive(list(list(category, genes, color)), or NULL)
#   custom_highlights,  # reactive(character vector) or NULL
#   fc_cutoff,          # reactive numeric (linear FC threshold, default 1.5)
#   q_cutoff,           # reactive numeric (q-value threshold, default 0.05)
#   title,              # reactive character
#   max_custom_labels,  # reactive integer (default 30)
#   label_font_size     # reactive integer (default 11)
# )
#
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
  dplyr[mutate, filter, case_when, arrange, desc, slice_head],
  utils[head],
)

ui <- function(id, height = "480px", width = "100%") {
  ns <- NS(id)
  shiny::div(
    plotly::plotlyOutput(ns("volcano_plot"), height = height, width = width),
    shiny::div(
      style = "font-size: 90%; color: #666; margin-top: 4px;",
      "Hover for details; click a point to capture the gene. Custom genes are labeled."
    )
  )
}

server <- function(
  id,
  dataset,
  timepoint,
  go_annotations = reactive(NULL),
  custom_highlights = reactive(NULL),
  fc_cutoff = reactive(1.5),
  q_cutoff = reactive(0.05),
  title = reactive("Volcano"),
  max_custom_labels = reactive(30),
  label_font_size = reactive(11)
) {
  stopifnot(is.character(timepoint), length(timepoint) == 1)

  moduleServer(id, function(input, output, session) {
    prepared <- reactive({
      req(dataset())
      df <- dataset()

      validate(
        need("Time_point" %in% names(df), "Time_point column missing."),
        need(
          any(c("Gene_single", "Gene_names") %in% names(df)),
          "Need Gene_single or Gene_names column."
        ),
        need("log2(Fold)" %in% names(df), "`log2(Fold)` column missing.")
      )

      gene_col <- if ("Gene_single" %in% names(df)) {
        "Gene_single"
      } else {
        "Gene_names"
      }
      sub <- df[df$Time_point == timepoint, , drop = FALSE]

      if (!nrow(sub)) {
        return(sub)
      }

      # Choose q_value if present; else p_value
      y_col <- if ("q_value" %in% names(sub)) {
        "q_value"
      } else if ("p_value" %in% names(sub)) {
        "p_value"
      } else {
        NULL
      }
      if (is.null(y_col)) {
        stop("Neither q_value nor p_value present in dataset.")
      }

      # Clean and guard
      sub[[y_col]][is.na(sub[[y_col]]) | sub[[y_col]] <= 0] <- min(
        sub[[y_col]][sub[[y_col]] > 0],
        na.rm = TRUE
      )
      sub[[y_col]][sub[[y_col]] > 1] <- 1

      sub$genes <- sub[[gene_col]]
      sub$log2FC <- sub[["log2(Fold)"]]
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
        metric_label = if (y_col == "q_value") "q-value" else "p-value",
        using_q = (y_col == "q_value")
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
                text = paste0("No data for time point: ", timepoint),
                x = 0.5,
                y = 0.5,
                xref = "paper",
                yref = "paper",
                showarrow = FALSE
              )
            )
        )
      }

      # Split data
      ns <- dat[dat$significance == "NotSig", , drop = FALSE]
      up <- dat[dat$significance == "Up", , drop = FALSE]
      dn <- dat[dat$significance == "Down", , drop = FALSE]

      base_cols <- list(
        NotSig = "#C7C7C7",
        Up = "#E74C3C",
        Down = "#1F77B4"
      )

      # Base (Not significant)
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
        showlegend = TRUE,
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
            showlegend = TRUE
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
            showlegend = TRUE
          )
      }

      # GO annotations (list of lists)
      go_list <- go_annotations()
      cat(
        "Volcano plot - GO annotations received:",
        if (is.null(go_list)) "NULL" else paste(length(go_list), "groups"),
        "\n"
      )

      if (!is.null(go_list) && length(go_list)) {
        # Debug: show what genes are available in this volcano data
        available_genes <- unique(dat$genes)
        cat(
          "Volcano plot - Available genes in dataset:",
          length(available_genes),
          "\n"
        )
        cat(
          "Volcano plot - Sample available genes:",
          paste(head(available_genes, 10), collapse = ", "),
          "\n"
        )

        for (i in seq_along(go_list)) {
          gl <- go_list[[i]]
          cat(
            "GO group",
            i,
            ":",
            gl$category,
            "with",
            length(gl$genes),
            "genes\n"
          )

          if (is.null(gl$category) || is.null(gl$genes) || is.null(gl$color)) {
            cat("Skipping GO group", i, "- missing data\n")
            next
          }

          # Debug: check gene name matching
          matching_genes <- intersect(gl$genes, available_genes)
          cat(
            "GO group",
            i,
            "- genes that match volcano data:",
            length(matching_genes),
            "\n"
          )
          if (length(matching_genes) > 0) {
            cat(
              "Sample matching genes:",
              paste(head(matching_genes, 5), collapse = ", "),
              "\n"
            )
          } else {
            cat(
              "NO MATCHING GENES - GO genes:",
              paste(head(gl$genes, 5), collapse = ", "),
              "\n"
            )
            cat(
              "Available genes sample:",
              paste(head(available_genes, 5), collapse = ", "),
              "\n"
            )
          }

          sub_go <- dat[dat$genes %in% gl$genes, , drop = FALSE]
          cat(
            "Found",
            nrow(sub_go),
            "matching genes in volcano data for",
            gl$category,
            "\n"
          )

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

      # Custom genes (with labels)
      custom_vec <- custom_highlights()
      if (!is.null(custom_vec) && length(custom_vec)) {
        sub_c <- dat[dat$genes %in% custom_vec, , drop = FALSE]
        if (nrow(sub_c)) {
          # Limit labels for readability
          max_labels <- max_custom_labels()
          if (nrow(sub_c) > max_labels) {
            sub_label <- sub_c |>
              arrange(desc(neg_log_metric)) |>
              slice_head(n = max_labels)
          } else {
            sub_label <- sub_c
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
            # Text labels trace using add_text instead of add_markers
            plotly::add_text(
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

      # Ranges and aspect
      x_rng <- range(dat$log2FC, na.rm = TRUE)
      y_rng <- range(dat$neg_log_metric, na.rm = TRUE)
      pad_x <- diff(x_rng) * 0.05
      pad_y <- diff(y_rng) * 0.05
      if (!is.finite(pad_x)) {
        pad_x <- 1
      }
      if (!is.finite(pad_y)) {
        pad_y <- 1
      }

      # Threshold lines
      fc_line <- log2(fc_cutoff())
      q_line_y <- -log10(q_cutoff())

      p <- p |>
        layout(
          title = list(text = title(), font = list(size = 18)),
          xaxis = list(
            title = "log2(Fold Change)",
            range = c(x_rng[1] - pad_x, x_rng[2] + pad_x),
            zeroline = TRUE,
            zerolinecolor = "rgba(0,0,0,0.25)"
          ),
          yaxis = list(
            title = "-log10(q-value)",
            range = c(max(0, y_rng[1] - pad_y), y_rng[2] + pad_y),
            scaleanchor = "x",
            scaleratio = 1
          ),
          legend = list(orientation = "h", y = -0.25),
          shapes = list(
            list(
              type = "line",
              x0 = fc_line,
              x1 = fc_line,
              y0 = 0,
              y1 = y_rng[2] + pad_y,
              line = list(color = "gray50", dash = "dash", width = 1)
            ),
            list(
              type = "line",
              x0 = -fc_line,
              x1 = -fc_line,
              y0 = 0,
              y1 = y_rng[2] + pad_y,
              line = list(color = "gray50", dash = "dash", width = 1)
            ),
            list(
              type = "line",
              x0 = x_rng[1] - pad_x,
              x1 = x_rng[2] + pad_x,
              y0 = q_line_y,
              y1 = q_line_y,
              line = list(color = "gray50", dash = "dash", width = 1)
            )
          )
        ) |>
        config(
          displaylogo = FALSE,
          modeBarButtonsToRemove = c(
            "lasso2d",
            "select2d",
            "zoomIn2d",
            "zoomOut2d",
            "autoScale2d"
          ),
          toImageButtonOptions = list(
            format = "png",
            filename = paste0("volcano_", timepoint),
            width = 1400,
            height = 1400
          )
        )

      p
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
