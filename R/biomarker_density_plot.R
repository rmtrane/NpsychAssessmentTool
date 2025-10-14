#' Plot density
#'
#' @description
#' A short description...
#'
#' @param obs A numeric value. Optional.
#' @param dens A list or data frame containing density `x` and `y` values.
#' @param cuts A numeric vector of cut points.
#' @param height A single numeric for the plot height. Optional.
#' @param width A single numeric for the plot width. Optional.
#'
#' @returns
#' A `plotly` object. Returns `NULL` invisibly if `obs` is `NA`.
#'
#' @export
density_plot <- function(
  obs = 30,
  dens = all_densities[[2]]$pTau_raw,
  cuts = all_cuts[[2]][name == "pTau", list(color, min_obs, max_obs)],
  height = 100,
  width = 400,
  new_id = NULL
) {
  # To avoid notes in R CMD check
  all_densities <- NULL
  all_cuts <- NULL
  name <- NULL
  color <- NULL
  min_obs <- NULL
  max_obs <- NULL

  if (is.na(obs)) {
    return()
  }

  density_df <- with(
    dens,
    data.frame(x = x, y = y)
  )

  percentile <- with(
    density_df[density_df$x <= obs, ],
    paste0(
      round(
        sum((x - data.table::shift(x)) * data.table::shift(y), na.rm = T) * 100,
        digits = 2
      ),
      "%"
    )
  )

  cut_values <- unique(unlist(cuts[, list(min_obs, max_obs)]))
  cuts_not_in_df <- cut_values[
    cut_values > 0 & is.finite(cut_values) & !cut_values %in% density_df$x
  ]

  if (length(cuts_not_in_df) > 0) {
    y_at_cuts <- sapply(cuts_not_in_df, \(cut) {
      mean(density_df$y[
        which(density_df$x == min(density_df$x[density_df$x > cut])) - c(0, 1)
      ])
    })
    density_df <- rbind(
      density_df,
      data.frame(x = cuts_not_in_df, y = y_at_cuts)
    )

    density_df <- density_df[order(density_df$x), ]
  }

  trace_colors <- cuts$color
  cuts <- cuts[, list(cuts = unique(c(min_obs, max_obs)))]$cuts

  obs_where <- findInterval(obs, vec = cuts)

  trace_colors[obs_where] <- gsub("alpha", "0.9", trace_colors[obs_where])
  trace_colors <- unlist(lapply(trace_colors, \(x) gsub("alpha", "0.4", x)))

  p <- plotly::plot_ly(
    data = density_df,
    type = "scatter",
    mode = "lines",
    height = height,
    width = width,
    hoverinfo = "none",
    source = "A"
  )

  # if (!is.null(new_id)) {
  #   names(p$x$visdat) <- new_id
  #   p$x$cur_data <- new_id

  #   names(p$x$attrs) <- new_id
  # }

  if (!is.null(new_id)) {
    names(p$x$visdat) <- new_id
    p$x$cur_data <- new_id

    names(p$x$attrs) <- new_id
    # names(p$x$layoutAttrs) <- new_id
  }

  for (i in seq_along(cuts)[-1]) {
    p <- p |>
      plotly::add_trace(
        x = density_df$x[density_df$x <= cuts[i] & density_df$x >= cuts[i - 1]],
        y = density_df$y[density_df$x <= cuts[i] & density_df$x >= cuts[i - 1]],
        fill = "tozeroy",
        fillcolor = trace_colors[i - 1],
        line = list(
          color = trace_colors[i - 1]
        )
      )
  }

  p <- p |>
    plotly::add_trace(
      mode = "markers",
      x = obs,
      y = 0,
      size = 15,
      marker = list(
        opacity = 1,
        color = "black",
        line = list(width = 0)
      ),
      # hoverinfo = "hovertext",,
      hoverlabel = list(
        align = "left"
      ),
      hovertemplate = paste0(
        "Observed value: ",
        round(obs, digits = 3),
        "<br>",
        "Percentile: ",
        percentile,
        "<extra></extra>"
      )
    ) |>
    plotly::add_trace(
      x = 0.95 * max(density_df$x),
      y = 0.95 * max(density_df$y),
      text = "&#8505;",
      mode = "text",
      hovertemplate = paste(
        "Density curve info<br> - Sample size:",
        dens$n,
        "<br> - Bandwidth:",
        dens$bw,
        "<extra></extra>"
      ),
      hoverlabel = list(
        align = "left"
      )
    ) |>
    plotly::layout(
      yaxis = list(
        showline = FALSE,
        showticklabels = FALSE,
        showgrid = FALSE,
        zeroline = TRUE,
        title = "" #,
        # range = c(-0.125, 1.05) * max(density_df$y)
      ),
      xaxis = list(
        zeroline = FALSE,
        showgrid = FALSE,
        title = "",
        range = c(-0.01, 1.01) * max(density_df$x),
        tickfont = list(size = 10)
      ),
      shapes = list(
        type = "line",
        x0 = cut,
        x1 = cut,
        y0 = 0,
        y1 = 1,
        yref = "paper",
        line = list(color = "grey45", dash = "dot")
      ),
      showlegend = F
    ) |>
    plotly::config(
      displayModeBar = FALSE,
      responsive = T
    )

  if (!is.null(new_id)) {
    p$elementId <- new_id
  }
  p
}
