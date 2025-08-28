library(shinytest2)

test_that("longTableModule", {
  for_tab <- demo_data |>
    subset(NACCID == "NACC017767") |>
    prepare_data()

  # Add "empty visit"
  empty_visit <- for_tab[
    1,
    lapply(.SD, \(x) NA),
    .SDcols = setdiff(colnames(for_tab), c("NACCID", "VISITDATE"))
  ]
  empty_visit$NACCID <- unique(for_tab$NACCID)
  empty_visit$VISITDATE <- min(for_tab$VISITDATE) - 365

  for_tab <- data.table::rbindlist(list(for_tab, empty_visit), use.names = TRUE)

  app <- AppDriver$new(
    app_dir = longTableApp(
      dat = for_tab
    ),
    variant = platform_variant(),
    name = "longTabMod1"
  )

  app$set_window_size(width = 1619, height = 968)

  app$expect_screenshot()

  app$click("long_table-show_hide_empty")

  app$wait_for_idle(timeout = 2000)

  app$expect_screenshot()

  app$stop()

  app <- AppDriver$new(
    app_dir = longTableApp(
      dat = for_tab,
      descriptions = shiny::reactiveVal(c(
        "Impaired" = 0.03,
        "Borderline" = 0.10,
        "Low Average" = 0.26,
        "Average" = 0.76,
        "High Average" = 0.92,
        "Superior" = 0.97,
        "Very Superior" = 1
      )),
      print_updating = T
    ),
    variant = platform_variant(),
    name = "longTabMod2"
  )

  app$set_window_size(width = 1619, height = 968)

  app$expect_screenshot()

  app$stop()
})
