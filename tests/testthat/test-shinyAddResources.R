test_that("shinyAddResources", {
  while (length(shiny::resourcePaths()) > 0) {
    shiny::removeResourcePath(names(shiny::resourcePaths())[1])
  }

  expect_error(
    shinyAddResources(development = T),
    regexp = "Couldn't normalize path in"
  )

  expect_null(
    suppressMessages(shinyAddResources(development = F))
  )

  expect_null(
    shinyAddResources(development = F)
  )

  ## Check side effects
  shinyAddResources(development = F)

  expect_equal(
    basename(shiny::resourcePaths()),
    c("www", "qmd")
  )

  # expect_equal(
  #   basename(dirname(shiny::resourcePaths())),
  #   c("inst", "inst")
  # )

  shiny::removeResourcePath(names(shiny::resourcePaths())[1])
  shiny::removeResourcePath(names(shiny::resourcePaths())[1])
})
