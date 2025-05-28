#' Create base plotly figure
#'
#' Create a base plot where traces can later be added.
#'
#' @param dat data object to get ranges for axes from
#' @param nacc_vars vector of variables to infer ranges from **without prefix**
#' @param shade_descriptions logical; should ranges for descriptions be shaded?
#' @param fill_alpha opacity value for shaded areas
#' @param source passed to `plotly::plot_ly`
#'
#' @inheritParams assessment_summary_table
#'
#' @keywords internal
#'
#' @export
base_plot_z_scores <- function(
  dat,
  nacc_vars = c(
    "TRAILA",
    "OTRAILA",
    "OTRLARR",
    "DIGFORCT",
    "DIGFORSL",
    "DIGBACCT",
    "DIGBACLS"
  ),
  descriptions = c(
    "Impaired" = 0.03,
    "Borderline" = 0.10,
    "Low Average" = 0.26,
    "Average" = 0.76,
    "High Average" = 0.92,
    "Superior" = 0.97,
    "Very Superior" = 1
  ),
  fill_values,
  shade_descriptions = T,
  fill_alpha = 0.2,
  source = "A"
) {
  stopifnot(
    "'dat' must be a data.table object" = data.table::is.data.table(dat)
  )

  ## Transform t-scores to normal scale
  t_score_cols <- names(which(
    lapply(dat, \(x) attributes(x)$method) == "T-score"
  ))

  for (col in t_score_cols) {
    dat[[col]] <- (dat[[col]] - 50) / 10
  }

  # dat[,
  #   names(.SD) := lapply(.SD, \(x) (x - 50) / 10),
  #   .SDcols = names(which(lapply(dat, \(x) attributes(x)$method) == "T-score"))
  # ]

  if (
    shade_descriptions &
      (missing(fill_values) || identical(fill_values, quote(expr = )))
  ) {
    fill_values <- setNames(
      # RColorBrewer::brewer.pal(n = length(descriptions), "RdYlGn"),
      calc_fill_colors(length(descriptions)),
      nm = names(descriptions) # c("Impaired", "Borderline", "Low Average", "Average", "High Average", "Superior", "Very Superior")
    )
  }

  if (
    (missing(nacc_vars) || identical(nacc_vars, quote(expr = ))) |
      is.null(nacc_vars)
  ) {
    most_extreme_val <- 2.5
  } else {
    if (sum(paste("std", nacc_vars, sep = "_") %in% colnames(dat)) == 0) {
      most_extreme_val <- 2.5
    } else {
      most_extreme_val <- dat[,
        setNames(
          nm = names(.SD),
          object = lapply(.SD, \(x) {
            if (all(is.na(x))) return(NA)

            max(abs(x), na.rm = T)
          })
        ),
        .SDcols = intersect(paste0("std_", nacc_vars), colnames(dat))
      ]

      most_extreme_val <- max(most_extreme_val, na.rm = T)
    }
  }
  ## Get min and max values for y-axis
  y_min <- min(-2.5, -most_extreme_val)
  y_max <- max(2.5, most_extreme_val)

  z_scores_from_percentiles <- qnorm(descriptions[descriptions < 1]) # c(0.03, 0.10, 0.25, 0.76, 0.92, 0.97))

  tiles <- data.table::data.table(
    ymin = c(y_min, z_scores_from_percentiles),
    ymax = c(z_scores_from_percentiles, y_max),
    descrs = names(descriptions)
  )

  # print(class(dat$VISITDATE))
  # print(date_range(as.Date(dat$VISITDATE)))

  p <- plotly::plot_ly(
    type = "scatter",
    mode = "none",
    x = date_range(dat$VISITDATE),
    showlegend = FALSE,
    hoverinfo = "none",
    source = source
  ) |>
    plotly::layout(
      xaxis = list(
        title = "Date of Test",
        range = date_range(dat$VISITDATE)
      ),
      yaxis = list(
        title = "z-score",
        range = list(y_min, y_max),
        showgrid = F
      ),
      legend = list(
        orientation = "h"
      )
    )

  ## Add horizontal lines and fill colors according to descriptions
  for (i in seq_along(fill_values)) {
    # seq_along(z_scores_from_percentiles)) {
    p <- p |>
      ## horizontal lines
      plotly::add_trace(
        y = z_scores_from_percentiles[min(
          i,
          length(z_scores_from_percentiles)
        )],
        type = "scatter",
        color = I(grDevices::rgb(0, 0, 0, 0.3)),
        alpha = I(as.numeric(i != length(fill_values))),
        mode = "lines",
        linetype = I("dashed"),
        line = list(
          width = 1
        ),
        hoverinfo = "none"
      ) |>
      ## fill
      plotly::add_trace(
        y = c(
          y_min,
          z_scores_from_percentiles[-length(z_scores_from_percentiles)],
          y_max
        )[i],
        type = "scatter",
        mode = "lines",
        fill = "tonexty",
        fillcolor = do.call(
          grDevices::rgb,
          as.list(c(
            grDevices::col2rgb(fill_values[i])[, 1] / 255,
            alpha = fill_alpha
          ))
        ),
        name = names(fill_values)[i],
        visible = shade_descriptions,
        line = list(
          width = 0
        ),
        hoverinfo = "none"
      )
  }

  p
}

#' @keywords internal
date_range <- function(dates) {
  dates <- as.Date(dates)

  c(
    lubridate::floor_date(min(dates, na.rm = T) - months(3), unit = "quarter"),
    lubridate::ceiling_date(max(dates, na.rm = T) + months(3), unit = "quarter")
  )
}
