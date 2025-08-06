#' Add Shiny resources
#'
#' @description
#' Adds resource paths for Shiny applications to access static files.
#'
#' @returns
#' No return value. This function is called for its side effects of adding
#' resource paths for Shiny applications, mapping `"www"` and `"qmd"`
#' to their respective directories within the package installation or
#' development environment.
#'
#' @export
shinyAddResources <- function() {
  development <- dir.exists("inst/www") &&
    basename(getwd()) == "NpsychAssessmentTool"

  if (development) {
    print("Development...")
    www_path <- "inst/www"
    qmd_path <- "inst/qmd"
  } else {
    require("NpsychAssessmentTool")

    www_path <- system.file("www", package = "NpsychAssessmentTool")
    qmd_path <- system.file("qmd", package = "NpsychAssessmentTool")
  }

  shiny::addResourcePath("www", www_path)
  shiny::addResourcePath("qmd", qmd_path)
}
