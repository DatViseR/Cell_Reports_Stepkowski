# Extended volcano module to support separate custom gene and GO category highlighting.
# Backward compatible with previous single highlight_genes approach.

box::use(
  shiny[moduleServer, NS, reactive, req],
  plotly[
    plot_ly,
    layout,
    event_data,
    add_markers,
    add_annotations,
    config,
    toRGB
  ],
  dplyr[mutate, case_when, filter],
  stats[quantile]
)

#' Volcano plot UI
#' @export
ui <- function(id, height = "600px", width = "100%") {
  ns <- NS(id)
  shiny::div(
    plotly::plotlyOutput(ns("volcano_plot"), height = height, width = width),
    shiny::div(
      style = "font-size: 90%; color: #666; margin-top: 10px;",
      shiny::p(
        shiny::span(
          shiny::tags$b("Hover"),
          " for details; ",
          shiny::span(shiny::tags$b("Click"), " to select a gene.")
        )
      )
    )
  )
}

#' Volcano plot server
#'
#' New (optional) parameters:
#'   - go_highlights: reactive(list of lists), each list element must contain:
#'       $category (character, GO label to show in legend)
#'       $genes    (character vector of genes in that GO category)
#'       $color    (hex color)
#'   - custom_highlights: reactive(character vector) of user custom genes (shown in black)
#'
#' If go_highlights/custom_highlights are provided they override the old single-group highlighting.
#' Old highlight_genes + highlight_info still work for backward compatibility.
#'
#' @export
server <- function(
  id,
  dataset,
  highlight_genes = reactive(NULL), # legacy single-group
  highlight_info = reactive(NULL), # legacy info block
  fc_cutoff = reactive(1),
  pval_cutoff = reactive(0.05),
  title = reactive("Volcano Plot"),
  go_highlights = reactive(NULL), # new multi-category GO highlights
  custom_highlights = reactive(NULL) # new custom user genes
) {
  moduleServer(id, function(input, output, session) {
    # Prepare main data
    plot_data <- reactive({
      req(dataset())
      data <- dataset()

      required_cols <- c("genes", "FC", "pval")
      if (!all(required_cols %in% names(data))) {
        missing <- required_cols[!required_cols %in% names(data)]
        stop(
          "Missing required columns in dataset: ",
          paste(missing, collapse = ", ")
        )
      }

      data <- mutate(
        data,
        neg_log_pval = -log10(pval),
        log2FC = if ("log2FC" %in% names(data)) data$log2FC else log2(FC),
        significance = case_when(
          abs(log2FC) >= log2(fc_cutoff()) & pval < pval_cutoff() ~
            ifelse(log2FC >= 0, "Upregulated", "Downregulated"),
          TRUE ~ "Not Significant"
        )
      )

      data
    })

    # Combined legacy highlight vector (union) if new system not used
    legacy_highlight_vector <- reactive({
      # Only used if new highlight system is absent
      if (!is.null(go_highlights()) || !is.null(custom_highlights())) {
        return(character())
      }
      highlight_genes() %||% character()
    })

    # Create plot
    output$volcano_plot <- plotly::renderPlotly({
      req(plot_data())
      dat <- plot_data()

      # Base significance colors
      sig_colors <- c(
        "Upregulated" = "#FF4136",
        "Downregulated" = "#0074D9",
        "Not Significant" = "#AAAAAA"
      )

      # Add legacy highlight flag (if using old API)
      dat$legacy_highlighted <- dat$genes %in% legacy_highlight_vector()

      # Base scatter (all genes)
      p <- plot_ly(
        data = dat,
        source = "volcano",
        x = ~log2FC,
        y = ~neg_log_pval,
        type = "scatter",
        mode = "markers",
        customdata = ~genes,
        text = ~ paste(
          "Gene:",
          genes,
          "<br>",
          "log2(FC):",
          round(log2FC, 3),
          "<br>",
          "p-value:",
          signif(pval, 3),
          if ("qvalue" %in% names(dat)) {
            paste("<br>q-value:", signif(dat$qvalue, 3))
          } else {
            ""
          }
        ),
        hoverinfo = "text",
        marker = list(
          color = sig_colors[dat$significance],
          size = 7,
          line = list(width = 0.3, color = "rgba(0,0,0,0.2)")
        ),
        showlegend = TRUE,
        legendgroup = "significance"
      )

      # Add legend dummy traces for significance categories (for consistent legend ordering)
      # (Optional: skip if not needed; significance colors already appear from base trace since color is direct vector.)
      # We'll skip extra dummy traces to keep code simpler.

      # If new multi-group highlighting is provided:
      go_list <- go_highlights()
      custom_vec <- custom_highlights()

      if (!is.null(go_list) || !is.null(custom_vec)) {
        # Normalize go_list structure
        if (!is.null(go_list) && length(go_list)) {
          for (i in seq_along(go_list)) {
            gl <- go_list[[i]]
            if (
              is.null(gl$category) || is.null(gl$genes) || is.null(gl$color)
            ) {
              next
            }

            # Remove genes that are also in custom set to avoid duplication
            genes_to_plot <- setdiff(
              unique(gl$genes),
              custom_vec %||% character()
            )
            if (!length(genes_to_plot)) {
              next
            }

            sub <- dat[dat$genes %in% genes_to_plot, , drop = FALSE]
            if (!nrow(sub)) {
              next
            }

            p <- p %>%
              add_markers(
                data = sub,
                x = ~log2FC,
                y = ~neg_log_pval,
                customdata = ~genes,
                text = ~ paste(
                  "Gene:",
                  genes,
                  "<br>",
                  "GO:",
                  gl$category,
                  "<br>",
                  "log2(FC):",
                  round(log2FC, 3),
                  "<br>",
                  "p-value:",
                  signif(pval, 3)
                ),
                hoverinfo = "text",
                marker = list(
                  color = gl$color,
                  size = 10,
                  line = list(width = 1, color = "black"),
                  symbol = "circle"
                ),
                name = gl$category,
                legendgroup = "GO",
                showlegend = TRUE
              )
          }
        }

        # Custom genes overlay (black)
        if (!is.null(custom_vec) && length(custom_vec)) {
          sub_custom <- dat[dat$genes %in% custom_vec, , drop = FALSE]
          # Avoid empty sub
          if (nrow(sub_custom)) {
            p <- p %>%
              add_markers(
                data = sub_custom,
                x = ~log2FC,
                y = ~neg_log_pval,
                customdata = ~genes,
                text = ~ paste(
                  "Gene:",
                  genes,
                  "<br>",
                  "Custom selection",
                  "<br>",
                  "log2(FC):",
                  round(log2FC, 3),
                  "<br>",
                  "p-value:",
                  signif(pval, 3)
                ),
                hoverinfo = "text",
                marker = list(
                  color = "#000000",
                  size = 11,
                  line = list(width = 1.5, color = "#FFFFFF"),
                  symbol = "diamond"
                ),
                name = "Custom genes",
                legendgroup = "Custom",
                showlegend = TRUE
              )
          }
        }
      } else {
        # Legacy single-group highlight (changes symbol/size)
        if (any(dat$legacy_highlighted)) {
          sub_h <- dat[dat$legacy_highlighted, , drop = FALSE]
          p <- p %>%
            add_markers(
              data = sub_h,
              x = ~log2FC,
              y = ~neg_log_pval,
              customdata = ~genes,
              text = ~ paste(
                "Gene:",
                genes,
                "<br>",
                "Highlighted set",
                "<br>",
                "log2(FC):",
                round(log2FC, 3),
                "<br>",
                "p-value:",
                signif(pval, 3)
              ),
              hoverinfo = "text",
              marker = list(
                color = "rgba(0,0,0,0)",
                size = 12,
                line = list(width = 2, color = "#333333"),
                symbol = "diamond"
              ),
              name = if (
                !is.null(highlight_info()) && !is.null(highlight_info()$name)
              ) {
                highlight_info()$name
              } else {
                "Highlighted genes"
              },
              legendgroup = "legacy",
              showlegend = TRUE
            )
        }
      }

      # Layout & cutoffs
      p <- p %>%
        layout(
          title = list(text = title(), font = list(size = 18)),
          xaxis = list(
            title = "log2(Fold Change)",
            zeroline = TRUE,
            zerolinecolor = toRGB("black", 0.4),
            zerolinewidth = 1
          ),
          yaxis = list(title = "-log10(p-value)"),
          legend = list(orientation = "h", y = -0.2),
          shapes = list(
            list(
              type = "line",
              x0 = log2(fc_cutoff()),
              x1 = log2(fc_cutoff()),
              y0 = 0,
              y1 = max(dat$neg_log_pval, na.rm = TRUE),
              line = list(color = "grey", dash = "dash")
            ),
            list(
              type = "line",
              x0 = -log2(fc_cutoff()),
              x1 = -log2(fc_cutoff()),
              y0 = 0,
              y1 = max(dat$neg_log_pval, na.rm = TRUE),
              line = list(color = "grey", dash = "dash")
            ),
            list(
              type = "line",
              x0 = min(dat$log2FC, na.rm = TRUE),
              x1 = max(dat$log2FC, na.rm = TRUE),
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

      # Legacy annotation info (kept for backward compatibility)
      if (
        is.null(go_highlights()) &&
          is.null(custom_highlights()) &&
          !is.null(highlight_info()) &&
          length(legacy_highlight_vector()) > 0
      ) {
        x_pos <- quantile(dat$log2FC, 0.85, na.rm = TRUE)
        y_pos <- quantile(dat$neg_log_pval, 0.85, na.rm = TRUE)
        info <- highlight_info()
        annotation_text <- paste0(
          "Highlighted: ",
          if ("name" %in% names(info)) info$name else "",
          if ("id" %in% names(info)) paste0(" (", info$id, ")") else "",
          "<br>",
          length(legacy_highlight_vector()),
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

      p
    })

    # Click selection returns gene symbol from customdata
    selected_genes <- reactive({
      ev <- event_data("plotly_click", source = "volcano")
      if (is.null(ev)) {
        return(NULL)
      }
      # ev$customdata is a list; convert first element or vector
      if (!is.null(ev$customdata)) {
        return(ev$customdata)
      }
      NULL
    })

    return(list(
      selected_genes = selected_genes,
      rendered_plot = reactive(output$volcano_plot)
    ))
  })
}
