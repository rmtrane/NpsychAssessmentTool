#' Colors for Lines on Plotly Charts
#'
#' Equivalent to `RColorBrewer::brewer.pal(n, "Paired")`
#'
#' @param n number of lines we need colors for
#'
#' @export
calc_line_colors <- function(n) {
  stopifnot("n must be at least 3" = n >= 3)
  stopifnot("n must be at most 12" = n <= 12)
  switch(
    n - 2,
    grDevices::rgb(
      c(166, 31, 178),
      c(206, 120, 223),
      c(227, 180, 138),
      maxColorValue = 255
    ),
    grDevices::rgb(
      c(166, 31, 178, 51),
      c(206, 120, 223, 160),
      c(227, 180, 138, 44),
      maxColorValue = 255
    ),
    grDevices::rgb(
      c(166, 31, 178, 51, 251),
      c(206, 120, 223, 160, 154),
      c(227, 180, 138, 44, 153),
      maxColorValue = 255
    ),
    grDevices::rgb(
      c(166, 31, 178, 51, 251, 227),
      c(206, 120, 223, 160, 154, 26),
      c(227, 180, 138, 44, 153, 28),
      maxColorValue = 255
    ),
    grDevices::rgb(
      c(166, 31, 178, 51, 251, 227, 253),
      c(206, 120, 223, 160, 154, 26, 191),
      c(227, 180, 138, 44, 153, 28, 111),
      maxColorValue = 255
    ),
    grDevices::rgb(
      c(166, 31, 178, 51, 251, 227, 253, 255),
      c(206, 120, 223, 160, 154, 26, 191, 127),
      c(227, 180, 138, 44, 153, 28, 111, 0),
      maxColorValue = 255
    ),
    grDevices::rgb(
      c(166, 31, 178, 51, 251, 227, 253, 255, 202),
      c(206, 120, 223, 160, 154, 26, 191, 127, 178),
      c(227, 180, 138, 44, 153, 28, 111, 0, 214),
      maxColorValue = 255
    ),
    grDevices::rgb(
      c(166, 31, 178, 51, 251, 227, 253, 255, 202, 106),
      c(206, 120, 223, 160, 154, 26, 191, 127, 178, 61),
      c(227, 180, 138, 44, 153, 28, 111, 0, 214, 154),
      maxColorValue = 255
    ),
    grDevices::rgb(
      c(166, 31, 178, 51, 251, 227, 253, 255, 202, 106, 255),
      c(206, 120, 223, 160, 154, 26, 191, 127, 178, 61, 255),
      c(227, 180, 138, 44, 153, 28, 111, 0, 214, 154, 153),
      maxColorValue = 255
    ),
    grDevices::rgb(
      c(166, 31, 178, 51, 251, 227, 253, 255, 202, 106, 255, 177),
      c(206, 120, 223, 160, 154, 26, 191, 127, 178, 61, 255, 89),
      c(227, 180, 138, 44, 153, 28, 111, 0, 214, 154, 153, 40),
      maxColorValue = 255
    )
  )
}
