library(shinytest2)

test_that("descriptionsModule works", {
  skip_on_cran()

  app <- AppDriver$new(
    app_dir = descriptionsApp(testing = TRUE),
    variant = platform_variant(),
    name = "descriptionsApp",
    clean_logs = FALSE,
    height = 968,
    width = 1619
  )
  app$set_window_size(width = 1619, height = 968)
  app$expect_screenshot()

  # fmt: skip
  app$set_inputs(
    `desc-descriptions_cell_clicked` = data.frame(row = 1, col = 2, value = 3),
    allow_no_input_binding_ = TRUE,
    priority_ = "event",
    wait_ = FALSE
  )
  # fmt: skip
  app$set_inputs(
    `desc-descriptions_cell_edit` = data.frame(row = 1, col = 2, value = 5),
    allow_no_input_binding_ = TRUE,
    priority_ = "event"
  )
  # fmt: skip
  app$set_inputs(
    `desc-descriptions_state` = c(1753207852487, 0, 10, "", TRUE, FALSE, TRUE, c(FALSE, "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE), c(FALSE, "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE)),
    allow_no_input_binding_ = TRUE,
    wait_ = F
  )
  # Update output value
  app$expect_screenshot()

  app$click("desc-add_row")
  app$wait_for_idle()

  app$expect_screenshot()

  app$set_inputs(
    `desc-descriptions_rows_current` = c(1, 2, 3, 4, 5, 6, 7, 8),
    allow_no_input_binding_ = TRUE,
    wait_ = FALSE
  )
  app$set_inputs(
    `desc-descriptions_rows_all` = c(1, 2, 3, 4, 5, 6, 7, 8),
    allow_no_input_binding_ = TRUE,
    wait_ = FALSE
  )
  # fmt: skip
  app$set_inputs(
    `desc-descriptions_state` = c(1753207863196, 0, 10, "", TRUE, FALSE, TRUE, c(FALSE, "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE), c(FALSE, "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE)),
    allow_no_input_binding_ = TRUE,
    wait_ = FALSE
  )
  # Update output value
  app$set_inputs(
    `desc-descriptions_cell_clicked` = data.frame(row = 8, col = 2),
    allow_no_input_binding_ = TRUE,
    priority_ = "event",
    wait_ = FALSE
  )
  app$set_inputs(
    `desc-descriptions_cell_clicked` = data.frame(row = 8, col = 2),
    allow_no_input_binding_ = TRUE,
    priority_ = "event",
    wait_ = FALSE
  )

  app$set_inputs(
    `desc-descriptions_cell_edit` = data.frame(row = 8, col = 2, value = 50),
    allow_no_input_binding_ = TRUE,
    priority_ = "event"
  )

  # fmt: skip
  app$set_inputs(
    `desc-descriptions_state` = c(1753207867565, 0, 10, "", TRUE, FALSE, TRUE, c(FALSE, "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE), c(FALSE, "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE)),
    allow_no_input_binding_ = TRUE,
    wait_ = FALSE
  )
  # Update output value
  app$expect_screenshot()

  # fmt: skip
  app$set_inputs(
    `desc-descriptions_cell_clicked` = data.frame(row = 4, col = 1, value = "New Group"),
    allow_no_input_binding_ = TRUE,
    priority_ = "event",
    wait_ = FALSE
  )
  # fmt: skip
  app$set_inputs(
    `desc-descriptions_cell_clicked` = data.frame(row = 4, col = 1, value = "New Group"),
    allow_no_input_binding_ = TRUE,
    priority_ = "event",
    wait_ = FALSE
  )

  # fmt: skip
  app$set_inputs(
    `desc-descriptions_cell_edit` = data.frame(row = 4, col = 1, value = "Median"),
    allow_no_input_binding_ = TRUE,
    priority_ = "event"
  )

  # fmt: skip
  app$set_inputs(
    `desc-descriptions_state` = c(1753207872049, 0, 10, "", TRUE, FALSE, TRUE, c(FALSE, "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE), c(FALSE, "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE)),
    allow_no_input_binding_ = TRUE,
    wait_ = FALSE
  )
  # Update output value
  app$expect_screenshot()

  # fmt: skip
  app$set_inputs(
    `desc-descriptions_cell_clicked` = data.frame(row = 7, col = 4, value = '<input type="color" id="color-6" value = "#91CF60">'),
    allow_no_input_binding_ = TRUE,
    priority_ = "event"
  )

  app$set_inputs(`desc-color-6` = "#080807", allow_no_input_binding_ = TRUE)
  app$set_inputs(
    `desc-newColorPicked` = "color-6",
    allow_no_input_binding_ = TRUE,
    priority_ = "event"
  )
  # fmt: skip
  app$set_inputs(
    `desc-descriptions_cell_clicked` = data.frame(row = 5, col = 5, value = '<i aria-label="remove icon" class="glyphicon glyphicon-remove" role="presentation"></i>'),
    allow_no_input_binding_ = TRUE,
    priority_ = "event"
  )
  # Update output value
  app$expect_screenshot()

  app$set_inputs(
    `desc-descriptions_rows_current` = c(1, 2, 3, 4, 5, 6, 7),
    allow_no_input_binding_ = TRUE,
    wait_ = FALSE
  )
  app$set_inputs(
    `desc-descriptions_rows_all` = c(1, 2, 3, 4, 5, 6, 7),
    allow_no_input_binding_ = TRUE,
    wait_ = FALSE
  )
  # fmt: skip
  app$set_inputs(
    `desc-descriptions_state` = c(1753207882070, 0, 10, "", TRUE, FALSE, TRUE, c(FALSE, "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE), c(FALSE, "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE)),
    allow_no_input_binding_ = TRUE,
    wait_ = FALSE
  )
  app$click("desc-reset")
  # Update output value
  # fmt: skip
  app$set_inputs(
    `desc-descriptions_state` = c(1753207884421, 0, 10, "", TRUE, FALSE, TRUE, c(FALSE, "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE), c(FALSE, "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE)),
    allow_no_input_binding_ = TRUE,
    wait_ = FALSE
  )

  app$expect_screenshot()
  app$stop()
})
