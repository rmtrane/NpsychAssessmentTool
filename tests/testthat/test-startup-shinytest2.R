library(shinytest2)

test_that("{shinytest2} testing initialization of shinyAssessmentTool", {
  skip_on_cran()

  app <- AppDriver$new(
    app_dir = shinyAssessmentApp(testing = T, use_mirai = F),
    variant = platform_variant(),
    name = "shinyApp",
    height = 968,
    width = 1619
  )

  app$set_inputs(
    `General Cognition-showPlot` = "yes",
    allow_no_input_binding_ = TRUE,
    wait_ = FALSE
  )
  app$set_inputs(
    `Attention/Processing-showPlot` = "yes",
    allow_no_input_binding_ = TRUE,
    wait_ = FALSE
  )
  app$set_inputs(
    `Language-showPlot` = "yes",
    allow_no_input_binding_ = TRUE,
    wait_ = FALSE
  )
  app$set_inputs(
    `Visuospatial-showPlot` = "yes",
    allow_no_input_binding_ = TRUE,
    wait_ = FALSE
  )
  app$set_inputs(
    `Memory-showPlot` = "yes",
    allow_no_input_binding_ = TRUE,
    wait_ = FALSE
  )
  app$set_inputs(
    `Executive Functioning-showPlot` = "yes",
    allow_no_input_binding_ = TRUE,
    wait_ = FALSE
  )
  app$set_inputs(
    `Mood-showPlot` = "yes",
    allow_no_input_binding_ = TRUE,
    wait_ = FALSE
  )
  app$set_window_size(width = 1619, height = 968)
  app$expect_values()
  app$stop()
})
