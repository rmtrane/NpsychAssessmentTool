#' Check colors
#'
#' @description
#' Check if all elements of a vector or list are valid hex colors.
#'
#' @param x A vector or list of elements
#' @param return_non_colors Optional. If `TRUE`, return all entries that are not recognized as hex colors as a named character vector where names give the entry number. If `FALSE` (default), return logical indicating if all entries are recognized as hex colors or not.
#'
#' @returns
#' Either a logical (if `return_non_colors` is `FALSE`) where `TRUE` indicates that all elements of `x` are valid hex colors, or a named character vector.
#'
#' @export
check_colors <- function(x, return_non_colors = F) {
  color_checks <- grepl(pattern = "^\\#[a-fA-F0-9]{6}$", x, perl = T)

  if (!return_non_colors) {
    return(all(color_checks))
  }

  wh <- which(!color_checks)

  setNames(x[wh], wh)
}
