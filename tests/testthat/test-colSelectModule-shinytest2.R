library(shinytest2)

test_that("errors", {
  expect_error(
    colSelectServer(
      id = "test",
      col_names = "test",
      default_methods = NpsychBatteryNorms::default_methods,
      data_type = "nacc",
      col_selection = "disable"
    ),
    regexp = "`col_names` must be a reactive"
  )

  expect_error(
    colSelectServer(
      id = "test",
      col_names = shiny::reactive("test"),
      default_methods = NpsychBatteryNorms::default_methods,
      data_type = "nacc",
      col_selection = "disable"
    ),
    regexp = "`data_type` must be a reactive"
  )
})

test_that("default cols and methods", {
  skip_on_cran()
  skip_on_ci()

  app <- AppDriver$new(
    app_dir = colSelectApp(
      col_names = shiny::reactive(colnames(demo_data)),
      default_methods = NpsychBatteryNorms::default_methods,
      data_type = shiny::reactive("nacc"),
      col_selection = "enable",
      testing = TRUE
    ),
    variant = platform_variant(),
    name = "sel_defaults",
    height = 968,
    width = 1619
  )

  app$click("colselect-assign")

  app$expect_values(export = "var_cols")

  app$stop()
})

test_that("col_selection='disable'", {
  skip_on_cran()
  skip_on_ci()

  app <- AppDriver$new(
    app_dir = colSelectApp(
      col_names = shiny::reactive(colnames(demo_data)),
      default_methods = NpsychBatteryNorms::default_methods,
      data_type = shiny::reactive("nacc"),
      col_selection = "disable",
      testing = TRUE
    ),
    variant = platform_variant(),
    name = "sel_disabled",
    height = 968,
    width = 1619,
    view = interactive()
  )

  app$set_inputs(
    `colselect-varstableDrawn` = "on",
    allow_no_input_binding_ = TRUE,
    wait_ = FALSE
  )
  # fmt: skip
  app$set_inputs(
    `colselect-vars_table_output_rows_current` = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 123, 124, 125, 126, 127, 128, 129, 130, 131, 132, 133, 134, 135, 136, 137, 138, 139, 140, 141, 142, 143, 144, 145, 146, 147, 148, 149, 150, 151, 152, 153, 154, 155, 156, 157, 158, 159, 160, 161),
    allow_no_input_binding_ = TRUE,
    wait_ = FALSE
  )
  # fmt: skip
  app$set_inputs(
    `colselect-vars_table_output_rows_all` = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 123, 124, 125, 126, 127, 128, 129, 130, 131, 132, 133, 134, 135, 136, 137, 138, 139, 140, 141, 142, 143, 144, 145, 146, 147, 148, 149, 150, 151, 152, 153, 154, 155, 156, 157, 158, 159, 160, 161),
    allow_no_input_binding_ = TRUE,
    wait_ = FALSE
  )
  # fmt: skip
  app$set_inputs(
    `colselect-vars_table_output_state` = c(1752076974603, 0, 10, "", TRUE, FALSE, TRUE, c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE), c(FALSE, "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE)),
    allow_no_input_binding_ = TRUE,
    wait_ = FALSE
  )
  # app$set_inputs(
  #   `colselect-MOCATOTSmethod` = "regression (nacc)",
  #   wait_ = FALSE
  # )
  # app$set_inputs(
  #   `colselect-NACCMMSEmethod` = "regression (nacc_legacy)",
  #   wait_ = FALSE
  # )
  # app$set_inputs(`colselect-TRAILAmethod` = "regression (nacc)", wait_ = FALSE)
  # app$set_inputs(
  #   `colselect-OTRAILAmethod` = "regression (updated_2025.06)",
  #   wait_ = FALSE
  # )
  # app$set_inputs(
  #   `colselect-OTRLARRmethod` = "regression (updated_2025.06)",
  #   wait_ = FALSE
  # )
  # app$set_inputs(
  #   `colselect-DIGFORCTmethod` = "regression (nacc)",
  #   wait_ = FALSE
  # )
  # app$set_inputs(
  #   `colselect-DIGFORSLmethod` = "regression (nacc)",
  #   wait_ = FALSE
  # )
  # app$set_inputs(
  #   `colselect-DIGBACCTmethod` = "regression (nacc)",
  #   wait_ = FALSE
  # )
  # app$set_inputs(
  #   `colselect-DIGBACLSmethod` = "regression (nacc)",
  #   wait_ = FALSE
  # )
  # app$set_inputs(`colselect-WAISmethod` = "T-score (NA)", wait_ = FALSE)
  # app$set_inputs(
  #   `colselect-DIGIFmethod` = "regression (nacc_legacy)",
  #   wait_ = FALSE
  # )
  # app$set_inputs(
  #   `colselect-DIGIFLENmethod` = "regression (nacc_legacy)",
  #   wait_ = FALSE
  # )
  # app$set_inputs(
  #   `colselect-DIGIBmethod` = "regression (nacc_legacy)",
  #   wait_ = FALSE
  # )
  # app$set_inputs(
  #   `colselect-DIGIBLENmethod` = "regression (nacc_legacy)",
  #   wait_ = FALSE
  # )
  # app$set_inputs(
  #   `colselect-MINTTOTSmethod` = "regression (nacc)",
  #   wait_ = FALSE
  # )
  # app$set_inputs(`colselect-ANIMALSmethod` = "regression (nacc)", wait_ = FALSE)
  # app$set_inputs(`colselect-VEGmethod` = "regression (nacc)", wait_ = FALSE)
  # app$set_inputs(
  #   `colselect-UDSVERTNmethod` = "regression (nacc)",
  #   wait_ = FALSE
  # )
  # app$set_inputs(
  #   `colselect-UDSVERFCmethod` = "regression (nacc)",
  #   wait_ = FALSE
  # )
  # app$set_inputs(
  #   `colselect-UDSVERLCmethod` = "regression (nacc)",
  #   wait_ = FALSE
  # )
  # app$set_inputs(
  #   `colselect-BOSTONmethod` = "regression (nacc_legacy)",
  #   wait_ = FALSE
  # )
  # app$set_inputs(
  #   `colselect-UDSBENTCmethod` = "regression (nacc)",
  #   wait_ = FALSE
  # )
  # app$set_inputs(
  #   `colselect-UDSBENTDmethod` = "regression (nacc)",
  #   wait_ = FALSE
  # )
  # app$set_inputs(
  #   `colselect-CRAFTVRSmethod` = "regression (nacc)",
  #   wait_ = FALSE
  # )
  # app$set_inputs(
  #   `colselect-CRAFTURSmethod` = "regression (nacc)",
  #   wait_ = FALSE
  # )
  # app$set_inputs(
  #   `colselect-CRAFTDVRmethod` = "regression (nacc)",
  #   wait_ = FALSE
  # )
  # app$set_inputs(
  #   `colselect-CRAFTDREmethod` = "regression (nacc)",
  #   wait_ = FALSE
  # )
  # app$set_inputs(`colselect-REY1RECmethod` = "T-score (NA)", wait_ = FALSE)
  # app$set_inputs(`colselect-REY2RECmethod` = "T-score (NA)", wait_ = FALSE)
  # app$set_inputs(`colselect-REY3RECmethod` = "T-score (NA)", wait_ = FALSE)
  # app$set_inputs(`colselect-REY4RECmethod` = "T-score (NA)", wait_ = FALSE)
  # app$set_inputs(`colselect-REY5RECmethod` = "T-score (NA)", wait_ = FALSE)
  # app$set_inputs(`colselect-REY6RECmethod` = "T-score (NA)", wait_ = FALSE)
  # app$set_inputs(`colselect-REYDRECmethod` = "T-score (NA)", wait_ = FALSE)
  # app$set_inputs(
  #   `colselect-LOGIMEMmethod` = "regression (nacc_legacy)",
  #   wait_ = FALSE
  # )
  # app$set_inputs(
  #   `colselect-MEMUNITSmethod` = "regression (nacc_legacy)",
  #   wait_ = FALSE
  # )
  # app$set_inputs(`colselect-REYTOTALmethod` = "T-score (NA)", wait_ = FALSE)
  # app$set_inputs(`colselect-REYARECmethod` = "T-score (NA)", wait_ = FALSE)
  # app$set_inputs(`colselect-TRAILBmethod` = "regression (nacc)", wait_ = FALSE)
  # app$set_inputs(
  #   `colselect-OTRAILBmethod` = "norms (updated)",
  #   wait_ = FALSE
  # )
  # app$set_inputs(
  #   `colselect-OTRLBRRmethod` = "norms (updated)",
  #   wait_ = FALSE
  # )
  app$expect_values()
  app$set_window_size(width = 1619, height = 968)
  app$expect_screenshot(
    threshold = 7
  )
  app$click("colselect-assign")

  app$wait_for_idle()

  app$expect_values()

  app$set_inputs(
    `colselect-MOCATOTSmethod` = "norms (nacc)",
    wait_ = FALSE
  )

  app$expect_values()
  app$set_window_size(width = 1619, height = 968)
  app$expect_screenshot()

  app$click("colselect-assign")

  app$wait_for_idle()

  app$expect_values()

  app$stop()
})

test_that("col_selection='enable'", {
  skip()
  skip_on_cran()
  skip_on_ci()

  app <- AppDriver$new(
    app_dir = colSelectApp(
      col_names = shiny::reactive(colnames(demo_data)),
      default_methods = NpsychBatteryNorms::default_methods,
      data_type = shiny::reactive("nacc"),
      col_selection = "enable",
      testing = TRUE
    ),
    variant = platform_variant(),
    name = "sel_enabled",
    height = 968,
    width = 1619,
    clean_logs = FALSE
  )

  # Update output value
  app$set_inputs(
    `colselect-varstableDrawn` = "on",
    allow_no_input_binding_ = TRUE,
    wait_ = FALSE
  )
  app$set_inputs(
    # fmt: skip
    `colselect-vars_table_output_rows_current` = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 123, 124, 125, 126, 127, 128, 129, 130, 131, 132, 133, 134, 135, 136, 137, 138, 139, 140, 141, 142, 143, 144, 145, 146, 147, 148, 149, 150, 151, 152, 153, 154, 155, 156, 157, 158, 159, 160, 161),
    allow_no_input_binding_ = TRUE,
    wait_ = FALSE
  )
  app$set_inputs(
    # fmt: skip
    `colselect-vars_table_output_rows_all` = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 123, 124, 125, 126, 127, 128, 129, 130, 131, 132, 133, 134, 135, 136, 137, 138, 139, 140, 141, 142, 143, 144, 145, 146, 147, 148, 149, 150, 151, 152, 153, 154, 155, 156, 157, 158, 159, 160, 161),
    allow_no_input_binding_ = TRUE,
    wait_ = FALSE
  )
  app$set_inputs(
    # fmt: skip
    `colselect-vars_table_output_state` = c(1753367312975, 0, 10, "", TRUE, FALSE, TRUE, c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE), c(FALSE, "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE)),
    allow_no_input_binding_ = TRUE,
    wait_ = FALSE
  )
  app$set_inputs(
    `colselect-MOCATOTSmethod` = "regression (nacc)",
    wait_ = FALSE
  )
  app$set_inputs(
    `colselect-NACCMMSEmethod` = "regression (nacc_legacy)",
    wait_ = FALSE
  )
  app$set_inputs(`colselect-TRAILAmethod` = "regression (nacc)", wait_ = FALSE)
  app$set_inputs(
    `colselect-OTRAILAmethod` = "regression (updated_2025.06)",
    wait_ = FALSE
  )
  app$set_inputs(
    `colselect-OTRLARRmethod` = "regression (updated_2025.06)",
    wait_ = FALSE
  )
  app$set_inputs(
    `colselect-DIGFORCTmethod` = "regression (nacc)",
    wait_ = FALSE
  )
  app$set_inputs(
    `colselect-DIGFORSLmethod` = "regression (nacc)",
    wait_ = FALSE
  )
  app$set_inputs(
    `colselect-DIGBACCTmethod` = "regression (nacc)",
    wait_ = FALSE
  )
  app$set_inputs(
    `colselect-DIGBACLSmethod` = "regression (nacc)",
    wait_ = FALSE
  )
  app$set_inputs(`colselect-WAISmethod` = "T-score (NA)", wait_ = FALSE)
  app$set_inputs(
    `colselect-DIGIFmethod` = "regression (nacc_legacy)",
    wait_ = FALSE
  )
  app$set_inputs(
    `colselect-DIGIFLENmethod` = "regression (nacc_legacy)",
    wait_ = FALSE
  )
  app$set_inputs(
    `colselect-DIGIBmethod` = "regression (nacc_legacy)",
    wait_ = FALSE
  )
  app$set_inputs(
    `colselect-DIGIBLENmethod` = "regression (nacc_legacy)",
    wait_ = FALSE
  )
  app$set_inputs(
    `colselect-MINTTOTSmethod` = "regression (nacc)",
    wait_ = FALSE
  )
  app$set_inputs(`colselect-ANIMALSmethod` = "regression (nacc)", wait_ = FALSE)
  app$set_inputs(`colselect-VEGmethod` = "regression (nacc)", wait_ = FALSE)
  app$set_inputs(
    `colselect-UDSVERTNmethod` = "regression (nacc)",
    wait_ = FALSE
  )
  app$set_inputs(
    `colselect-UDSVERFCmethod` = "regression (nacc)",
    wait_ = FALSE
  )
  app$set_inputs(
    `colselect-UDSVERLCmethod` = "regression (nacc)",
    wait_ = FALSE
  )
  app$set_inputs(
    `colselect-BOSTONmethod` = "regression (nacc_legacy)",
    wait_ = FALSE
  )
  app$set_inputs(
    `colselect-UDSBENTCmethod` = "regression (nacc)",
    wait_ = FALSE
  )
  app$set_inputs(
    `colselect-UDSBENTDmethod` = "regression (nacc)",
    wait_ = FALSE
  )
  app$set_inputs(
    `colselect-CRAFTVRSmethod` = "regression (nacc)",
    wait_ = FALSE
  )
  app$set_inputs(
    `colselect-CRAFTURSmethod` = "regression (nacc)",
    wait_ = FALSE
  )
  app$set_inputs(
    `colselect-CRAFTDVRmethod` = "regression (nacc)",
    wait_ = FALSE
  )
  app$set_inputs(
    `colselect-CRAFTDREmethod` = "regression (nacc)",
    wait_ = FALSE
  )
  app$set_inputs(`colselect-REY1RECmethod` = "T-score (NA)", wait_ = FALSE)
  app$set_inputs(`colselect-REY2RECmethod` = "T-score (NA)", wait_ = FALSE)
  app$set_inputs(`colselect-REY3RECmethod` = "T-score (NA)", wait_ = FALSE)
  app$set_inputs(`colselect-REY4RECmethod` = "T-score (NA)", wait_ = FALSE)
  app$set_inputs(`colselect-REY5RECmethod` = "T-score (NA)", wait_ = FALSE)
  app$set_inputs(`colselect-REY6RECmethod` = "T-score (NA)", wait_ = FALSE)
  app$set_inputs(`colselect-REYDRECmethod` = "T-score (NA)", wait_ = FALSE)
  app$set_inputs(
    `colselect-LOGIMEMmethod` = "regression (nacc_legacy)",
    wait_ = FALSE
  )
  app$set_inputs(
    `colselect-MEMUNITSmethod` = "regression (nacc_legacy)",
    wait_ = FALSE
  )
  app$set_inputs(`colselect-REYTOTALmethod` = "T-score (NA)", wait_ = FALSE)
  app$set_inputs(`colselect-REYARECmethod` = "T-score (NA)", wait_ = FALSE)
  app$set_inputs(`colselect-TRAILBmethod` = "regression (nacc)", wait_ = FALSE)
  app$set_inputs(
    `colselect-OTRAILBmethod` = "regression (updated_2025.06)",
    wait_ = FALSE
  )
  app$set_inputs(
    `colselect-OTRLBRRmethod` = "regression (updated_2025.06)",
    wait_ = FALSE
  )
  app$set_window_size(width = 1619, height = 968)
  app$set_inputs(
    "colselect-vars_table_output_cells_selected" = data.frame(row = 1, col = 5),
    allow_no_input_binding_ = TRUE,
    wait_ = FALSE
  )
  app$set_inputs(
    `colselect-vars_table_output_cell_clicked` = data.frame(
      row = "1",
      col = "5",
      value = "NACCID"
    ),
    allow_no_input_binding_ = TRUE,
    priority_ = "event",
    wait_ = FALSE
  )
  app$click("colselect-change_column")
  # Update output value
  app$set_inputs(`colselect-newColumn` = "NACCID")
  app$expect_screenshot()
  app$set_inputs(`colselect-newColumn` = "VASC")
  app$click("colselect-change_column")
  app$set_inputs(
    `colselect-vars_table_output_cells_selected` = character(0),
    allow_no_input_binding_ = TRUE
  )
  app$set_inputs(
    `colselect-vars_table_output_state` = c(
      1753367371586,
      0,
      10,
      "",
      TRUE,
      FALSE,
      TRUE,
      c(TRUE, "", TRUE, FALSE, TRUE),
      c(TRUE, "", TRUE, FALSE, TRUE),
      c(FALSE, "", TRUE, FALSE, TRUE),
      c(TRUE, "", TRUE, FALSE, TRUE),
      c(TRUE, "", TRUE, FALSE, TRUE),
      c(TRUE, "", TRUE, FALSE, TRUE),
      c(TRUE, "", TRUE, FALSE, TRUE)
    ),
    allow_no_input_binding_ = TRUE
  )
  app$expect_values()
  app$set_inputs(
    `colselect-vars_table_output_cells_selected` = c(1, 5),
    allow_no_input_binding_ = TRUE
  )
  app$set_inputs(
    `colselect-vars_table_output_cell_clicked` = c("1", "5", "VASC"),
    allow_no_input_binding_ = TRUE,
    priority_ = "event"
  )
  app$set_inputs(
    `colselect-selectizeInputUpdated` = "on",
    allow_no_input_binding_ = TRUE,
    priority_ = "event"
  )
  app$set_inputs(`colselect-newColumn` = "(blank)")
  app$click("colselect-change_column")
  app$set_inputs(`colselect-newColumn` = "VASC")
  app$set_inputs(`colselect-newColumn` = "NACCID")
  app$click("colselect-change_column")
  app$set_inputs(
    `colselect-vars_table_output_cells_selected` = character(0),
    allow_no_input_binding_ = TRUE
  )
  app$set_inputs(
    `colselect-vars_table_output_state` = c(
      1753367383770,
      0,
      10,
      "",
      TRUE,
      FALSE,
      TRUE,
      c(TRUE, "", TRUE, FALSE, TRUE),
      c(TRUE, "", TRUE, FALSE, TRUE),
      c(FALSE, "", TRUE, FALSE, TRUE),
      c(TRUE, "", TRUE, FALSE, TRUE),
      c(TRUE, "", TRUE, FALSE, TRUE),
      c(TRUE, "", TRUE, FALSE, TRUE),
      c(TRUE, "", TRUE, FALSE, TRUE)
    ),
    allow_no_input_binding_ = TRUE
  )
  app$set_inputs(
    `colselect-vars_table_output_cells_selected` = c(117, 5),
    allow_no_input_binding_ = TRUE
  )
  app$set_inputs(
    `colselect-vars_table_output_cell_clicked` = c("117", "5", "(blank)"),
    allow_no_input_binding_ = TRUE,
    priority_ = "event"
  )
  app$set_inputs(
    `colselect-selectizeInputUpdated` = "on",
    allow_no_input_binding_ = TRUE,
    priority_ = "event"
  )
  app$set_inputs(`colselect-newColumn` = "(blank)")
  app$click("colselect-change_column")
  app$set_inputs(`colselect-newColumn` = "(blank)")
  app$set_inputs(`colselect-newColumn` = "")
  app$set_inputs(`colselect-newColumn` = "VASC")
  app$click("colselect-change_column")
  app$set_inputs(
    `colselect-vars_table_output_cells_selected` = character(0),
    allow_no_input_binding_ = TRUE
  )
  app$set_inputs(
    `colselect-vars_table_output_state` = c(
      1753367399819,
      0,
      10,
      "",
      TRUE,
      FALSE,
      TRUE,
      c(TRUE, "", TRUE, FALSE, TRUE),
      c(TRUE, "", TRUE, FALSE, TRUE),
      c(FALSE, "", TRUE, FALSE, TRUE),
      c(TRUE, "", TRUE, FALSE, TRUE),
      c(TRUE, "", TRUE, FALSE, TRUE),
      c(TRUE, "", TRUE, FALSE, TRUE),
      c(TRUE, "", TRUE, FALSE, TRUE)
    ),
    allow_no_input_binding_ = TRUE
  )
  app$expect_values()
  app$click("colselect-assign")
  app$expect_values()

  app$stop()
})


test_that("col_selection='hide'", {
  skip_on_cran()
  skip_on_ci()

  app <- AppDriver$new(
    app_dir = colSelectApp(
      col_names = shiny::reactive(colnames(demo_data)),
      default_methods = NpsychBatteryNorms::default_methods,
      data_type = shiny::reactive("nacc"),
      col_selection = "hide",
      testing = TRUE
    ),
    variant = platform_variant(),
    name = "sel_hidden",
    height = 968,
    width = 1619
  )

  app$set_inputs(
    `colselect-varstableDrawn` = "on",
    allow_no_input_binding_ = TRUE,
    wait_ = FALSE
  )
  # fmt: skip
  app$set_inputs(
    `colselect-vars_table_output_rows_current` = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 123, 124, 125, 126, 127, 128, 129, 130, 131, 132, 133, 134, 135, 136, 137, 138, 139, 140, 141, 142, 143, 144, 145, 146, 147, 148, 149, 150, 151, 152, 153, 154, 155, 156, 157, 158, 159, 160, 161),
    allow_no_input_binding_ = TRUE,
    wait_ = FALSE
  )
  # fmt: skip
  app$set_inputs(
    `colselect-vars_table_output_rows_all` = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 123, 124, 125, 126, 127, 128, 129, 130, 131, 132, 133, 134, 135, 136, 137, 138, 139, 140, 141, 142, 143, 144, 145, 146, 147, 148, 149, 150, 151, 152, 153, 154, 155, 156, 157, 158, 159, 160, 161),
    allow_no_input_binding_ = TRUE,
    wait_ = FALSE
  )
  # fmt: skip
  app$set_inputs(
    `colselect-vars_table_output_state` = c(1752076974603, 0, 10, "", TRUE, FALSE, TRUE, c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE), c(FALSE, "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE), c(TRUE, "", TRUE, FALSE, TRUE)),
    allow_no_input_binding_ = TRUE,
    wait_ = FALSE
  )
  app$set_inputs(
    `colselect-MOCATOTSmethod` = "regression (nacc)",
    wait_ = FALSE
  )
  app$set_inputs(
    `colselect-NACCMMSEmethod` = "regression (nacc_legacy)",
    wait_ = FALSE
  )
  app$set_inputs(`colselect-TRAILAmethod` = "regression (nacc)", wait_ = FALSE)
  app$set_inputs(
    `colselect-OTRAILAmethod` = "regression (updated_2025.06)",
    wait_ = FALSE
  )
  app$set_inputs(
    `colselect-OTRLARRmethod` = "regression (updated_2025.06)",
    wait_ = FALSE
  )
  app$set_inputs(
    `colselect-DIGFORCTmethod` = "regression (nacc)",
    wait_ = FALSE
  )
  app$set_inputs(
    `colselect-DIGFORSLmethod` = "regression (nacc)",
    wait_ = FALSE
  )
  app$set_inputs(
    `colselect-DIGBACCTmethod` = "regression (nacc)",
    wait_ = FALSE
  )
  app$set_inputs(
    `colselect-DIGBACLSmethod` = "regression (nacc)",
    wait_ = FALSE
  )
  app$set_inputs(`colselect-WAISmethod` = "T-score (NA)", wait_ = FALSE)
  app$set_inputs(
    `colselect-DIGIFmethod` = "regression (nacc_legacy)",
    wait_ = FALSE
  )
  app$set_inputs(
    `colselect-DIGIFLENmethod` = "regression (nacc_legacy)",
    wait_ = FALSE
  )
  app$set_inputs(
    `colselect-DIGIBmethod` = "regression (nacc_legacy)",
    wait_ = FALSE
  )
  app$set_inputs(
    `colselect-DIGIBLENmethod` = "regression (nacc_legacy)",
    wait_ = FALSE
  )
  app$set_inputs(
    `colselect-MINTTOTSmethod` = "regression (nacc)",
    wait_ = FALSE
  )
  app$set_inputs(`colselect-ANIMALSmethod` = "regression (nacc)", wait_ = FALSE)
  app$set_inputs(`colselect-VEGmethod` = "regression (nacc)", wait_ = FALSE)
  app$set_inputs(
    `colselect-UDSVERTNmethod` = "regression (nacc)",
    wait_ = FALSE
  )
  app$set_inputs(
    `colselect-UDSVERFCmethod` = "regression (nacc)",
    wait_ = FALSE
  )
  app$set_inputs(
    `colselect-UDSVERLCmethod` = "regression (nacc)",
    wait_ = FALSE
  )
  app$set_inputs(
    `colselect-BOSTONmethod` = "regression (nacc_legacy)",
    wait_ = FALSE
  )
  app$set_inputs(
    `colselect-UDSBENTCmethod` = "regression (nacc)",
    wait_ = FALSE
  )
  app$set_inputs(
    `colselect-UDSBENTDmethod` = "regression (nacc)",
    wait_ = FALSE
  )
  app$set_inputs(
    `colselect-CRAFTVRSmethod` = "regression (nacc)",
    wait_ = FALSE
  )
  app$set_inputs(
    `colselect-CRAFTURSmethod` = "regression (nacc)",
    wait_ = FALSE
  )
  app$set_inputs(
    `colselect-CRAFTDVRmethod` = "regression (nacc)",
    wait_ = FALSE
  )
  app$set_inputs(
    `colselect-CRAFTDREmethod` = "regression (nacc)",
    wait_ = FALSE
  )
  app$set_inputs(`colselect-REY1RECmethod` = "T-score (NA)", wait_ = FALSE)
  app$set_inputs(`colselect-REY2RECmethod` = "T-score (NA)", wait_ = FALSE)
  app$set_inputs(`colselect-REY3RECmethod` = "T-score (NA)", wait_ = FALSE)
  app$set_inputs(`colselect-REY4RECmethod` = "T-score (NA)", wait_ = FALSE)
  app$set_inputs(`colselect-REY5RECmethod` = "T-score (NA)", wait_ = FALSE)
  app$set_inputs(`colselect-REY6RECmethod` = "T-score (NA)", wait_ = FALSE)
  app$set_inputs(`colselect-REYDRECmethod` = "T-score (NA)", wait_ = FALSE)
  app$set_inputs(
    `colselect-LOGIMEMmethod` = "regression (nacc_legacy)",
    wait_ = FALSE
  )
  app$set_inputs(
    `colselect-MEMUNITSmethod` = "regression (nacc_legacy)",
    wait_ = FALSE
  )
  app$set_inputs(`colselect-REYTOTALmethod` = "T-score (NA)", wait_ = FALSE)
  app$set_inputs(`colselect-REYARECmethod` = "T-score (NA)", wait_ = FALSE)
  app$set_inputs(`colselect-TRAILBmethod` = "regression (nacc)", wait_ = FALSE)
  app$set_inputs(
    `colselect-OTRAILBmethod` = "regression (updated_2025.06)",
    wait_ = FALSE
  )
  app$set_inputs(
    `colselect-OTRLBRRmethod` = "regression (updated_2025.06)",
    wait_ = FALSE
  )
  app$expect_values()
  app$set_window_size(width = 1619, height = 968)
  app$expect_screenshot()
  app$click("colselect-assign")

  app$wait_for_idle()

  app$expect_values()

  app$set_inputs(
    `colselect-MOCATOTSmethod` = "norms (nacc)",
    wait_ = FALSE
  )

  app$expect_values()
  app$set_window_size(width = 1619, height = 968)
  app$expect_screenshot()

  app$click("colselect-assign")

  app$wait_for_idle()

  app$expect_values()

  app$stop()
})
