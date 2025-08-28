library(shinytest2)

test_that("mainTableModule works as expected", {
  skip_on_cran()
  skip_on_ci()

  app <- AppDriver$new(
    app_dir = mainTableApp(
      dat = demo_data |>
        prepare_data(selected_cols = NULL) |>
        dplyr::filter(NACCID == "NACC017767") |>
        dplyr::filter(VISITDATE == max(VISITDATE)),
      testing = TRUE
    ),
    variant = platform_variant(),
    name = "mainTable",
    height = 968,
    width = 1619
  )

  app$wait_for_idle()

  app$expect_screenshot()

  app$click("main_table-genPDF")
  app$wait_for_idle()

  app$expect_screenshot()

  app$stop()
})

test_that("mainTableModule handles NULL dat", {
  skip_on_cran()
  skip_on_ci()

  app <- AppDriver$new(
    app_dir = mainTableApp(
      dat = NULL,
      testing = TRUE
    ),
    variant = platform_variant(),
    name = "mainTable-NULL",
    height = 968,
    width = 1619
  )

  app$wait_for_idle()

  app$expect_screenshot()

  app$stop()
})
