#' Add Shiny resources
#'
#' @description
#' Adds resource paths for Shiny applications to access static files.
#'
#' @param development logical; indicading if run in development mode. If `TRUE`, resources will be pulled from `inst/www` and `inst/qmd`. Otherwise, the path for the installed package will be used.
#'
#' @returns
#' No return value. This function is called for its side effects of adding
#' resource paths for Shiny applications, mapping `"www"` and `"qmd"`
#' to their respective directories within the package installation or
#' development environment.
#'
#' @keywords internal
#' @export
shinyAddResources <- function(development) {
  if (missingArg(development)) {
    development <- dir.exists("inst/www") &&
      basename(getwd()) == "NpsychAssessmentTool"
  }

  if (development) {
    cli::cli_inform("Running in development mode...")
    www_path <- "./inst/www"
    qmd_path <- "./inst/qmd"
  } else {
    require("NpsychAssessmentTool")

    www_path <- system.file("www", package = "NpsychAssessmentTool")
    qmd_path <- system.file("qmd", package = "NpsychAssessmentTool")
  }

  shiny::addResourcePath("www", www_path)
  shiny::addResourcePath("qmd", qmd_path)

  return()
}
