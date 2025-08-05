library(shinytest2)

test_that("prevDiagnosesModule", {
  skip_on_cran()
  skip_on_ci()

  tmp_dat <- demo_data |>
    prepare_data(
      selected_cols = colnames(demo_data),
      print_messages = F,
      with_diags = T
    )

  app <- AppDriver$new(
    app = prevDiagnosesApp(
      dat = tmp_dat |> subset(NACCID == "NACC036131")
    ),
    variant = platform_variant(),
    height = 968,
    width = 1619,
    wait = TRUE
  )

  app$wait_for_idle()

  app$expect_screenshot()
  app$stop()

  app <- AppDriver$new(
    app = prevDiagnosesApp(
      dat = tmp_dat |> subset(NACCID == "NACC036131"),
      print_updating = TRUE
    ),
    variant = platform_variant(),
    height = 968,
    width = 1619,
    wait = TRUE
  )

  app$wait_for_idle()

  app$expect_screenshot()

  app$stop()
})
