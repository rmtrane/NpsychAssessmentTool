#' Run Shiny App
#'
#' @description
#' Wrapper that runs the Shiny application.
#'
#' @returns
#' Runs the Shiny app.
#'
#' @export
shinyAssessmentApp <- function() {
  options(shiny.maxRequestSize = 1000 * 1024^2)

  development <- dir.exists("inst/www") &&
    basename(getwd()) == "NpsychAssessmentTool"

  if (development) {
    print("Development...")
    www_path <- "inst/www"
    qmd_path <- "inst/qmd"
  } else {
    www_path <- system.file("www", package = "NpsychAssessmentTool")
    qmd_path <- system.file("qmd", package = "NpsychAssessmentTool")
  }

  shiny::addResourcePath("www", www_path)
  shiny::addResourcePath("qmd", qmd_path)

  shiny::shinyApp(
    ui = appUI,
    server = appServer,
    options = list(
      port = 5556,
      launch.browser = TRUE
    )
  )
}
