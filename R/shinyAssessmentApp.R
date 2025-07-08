#' Run Shiny App
#'
#' @description
#' Wrapper that runs the Shiny application.
#'
#' @param testing Logical: is the app run for testing purposes?
#' @param use_mirai Logical: should `"mirai"` be used for asynchronously
#' pulling biomarker data. Defaults to `rlang::is_installed("mirai")`.
#'
#' @returns
#' Runs the Shiny app.
#'
#' @export
shinyAssessmentApp <- function(
  testing = FALSE,
  use_mirai = rlang::is_installed("mirai")
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

  if (rlang::is_installed("mirai") && use_mirai) {
    mirai::daemons(1)

    shiny::onStop(\(x) mirai::daemons(0))
  }

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
