#' Run Shiny App
#'
#' @description
#' Wrapper that runs the Shiny application.
#'
#' @param testing Logical: is the app run for testing purposes?
#'
#' @returns
#' Runs the Shiny app.
#'
#' @export
shinyAssessmentApp <- function(
  testing = FALSE
) {
  options(
    shiny.maxRequestSize = 1000 * 1024^2,
    shiny.autoload.r = FALSE
  )

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

  if (mirai::daemons_set()) {
    mirai::daemons(0)
  }

  mirai::daemons(1)
  shiny::onStop(\(x) mirai::daemons(0))

  shiny::shinyApp(
    ui = appUI,
    server = appServer,
    options = list(
      port = 5556,
      launch.browser = TRUE,
      test.mode = testing
    )
  )
}
