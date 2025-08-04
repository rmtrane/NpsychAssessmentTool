library(shinytest2)

test_that("mainTableModule works as expected", {
  skip_on_cran()

  app <- AppDriver$new(
    app_dir = mainTableApp(
      dat = demo_data |>
        prepare_data(selected_cols = names(demo_data)) |>
        dplyr::filter(NACCID == "NACC097067") |>
        dplyr::filter(VISITYR == max(VISITYR)),
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
