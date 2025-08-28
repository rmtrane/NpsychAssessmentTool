library(shinytest2)

test_that("screenshots for introduction", {
  skip_on_cran()
  skip_on_ci()

  app <- AppDriver$new(
    app_dir = shinyAssessmentApp(testing = T),
    variant = platform_variant(),
    name = "shinyApp",
    height = 968,
    width = 1619,
    wait = TRUE,
    view = interactive()
  )

  # Update unbound `input` value
  app$set_window_size(width = 1920, height = 1080)
  app$set_inputs(main_navbar = "dataSelect")
  # Update output value
  app$set_window_size(width = 1920, height = 1080)
  app$wait_for_idle()
  app$expect_screenshot(
    screenshot_args = list(selector = "viewport")
  )
  app$set_inputs(`dataSelect-data_source` = "demo")
  # Update output value
  app$click('dataSelect-fetch_data_button')
  app$wait_for_idle()
  # Update output value
  app$expect_screenshot(
    screenshot_args = list(selector = "viewport")
  )
  app$click("dataSelect-submit")
  app$wait_for_idle()
  # Update output value
  # Update unbound `input` value
  # fmt: skip
  app$set_inputs(`colSelect-MOCATOTSmethod` = "regression (nacc)", wait_ = FALSE)
  # fmt: skip
  app$set_inputs(`colSelect-NACCMMSEmethod` = "regression (nacc_legacy)", wait_ = FALSE)
  # fmt: skip
  app$set_inputs(`colSelect-TRAILAmethod` = "regression (nacc)", wait_ = FALSE)
  # fmt: skip
  app$set_inputs(`colSelect-OTRAILAmethod` = "regression (updated_2025.06)", wait_ = FALSE)
  # fmt: skip
  app$set_inputs(`colSelect-DIGFORCTmethod` = "regression (nacc)", wait_ = FALSE)
  # fmt: skip
  app$set_inputs(`colSelect-DIGFORSLmethod` = "regression (nacc)", wait_ = FALSE)
  # fmt: skip
  app$set_inputs(`colSelect-DIGBACCTmethod` = "regression (nacc)", wait_ = FALSE)
  # fmt: skip
  app$set_inputs(`colSelect-DIGBACLSmethod` = "regression (nacc)", wait_ = FALSE)
  # fmt: skip
  app$set_inputs(`colSelect-WAISmethod` = "T-score (NA)", wait_ = FALSE)
  # fmt: skip
  app$set_inputs(`colSelect-DIGIFmethod` = "regression (nacc_legacy)", wait_ = FALSE)
  # fmt: skip
  app$set_inputs(`colSelect-DIGIFLENmethod` = "regression (nacc_legacy)", wait_ = FALSE)
  # fmt: skip
  app$set_inputs(`colSelect-DIGIBmethod` = "regression (nacc_legacy)", wait_ = FALSE)
  # fmt: skip
  app$set_inputs(`colSelect-DIGIBLENmethod` = "regression (nacc_legacy)", wait_ = FALSE)
  # fmt: skip
  app$set_inputs(`colSelect-MINTTOTSmethod` = "regression (nacc)", wait_ = FALSE)
  # fmt: skip
  app$set_inputs(`colSelect-ANIMALSmethod` = "regression (nacc)", wait_ = FALSE)
  # fmt: skip
  app$set_inputs(`colSelect-VEGmethod` = "regression (nacc)", wait_ = FALSE)
  # fmt: skip
  app$set_inputs(`colSelect-UDSVERTNmethod` = "regression (nacc)", wait_ = FALSE)
  # fmt: skip
  app$set_inputs(`colSelect-UDSVERFCmethod` = "regression (nacc)", wait_ = FALSE)
  # fmt: skip
  app$set_inputs(`colSelect-UDSVERLCmethod` = "regression (nacc)", wait_ = FALSE)
  # fmt: skip
  app$set_inputs(`colSelect-BOSTONmethod` = "regression (nacc_legacy)", wait_ = FALSE)
  # fmt: skip
  app$set_inputs(`colSelect-UDSBENTCmethod` = "regression (nacc)", wait_ = FALSE)
  # fmt: skip
  app$set_inputs(`colSelect-UDSBENTDmethod` = "regression (nacc)", wait_ = FALSE)
  # fmt: skip
  app$set_inputs(`colSelect-CRAFTVRSmethod` = "regression (nacc)", wait_ = FALSE)
  # fmt: skip
  app$set_inputs(`colSelect-CRAFTURSmethod` = "regression (nacc)", wait_ = FALSE)
  # fmt: skip
  app$set_inputs(`colSelect-CRAFTDVRmethod` = "regression (nacc)", wait_ = FALSE)
  # fmt: skip
  app$set_inputs(`colSelect-CRAFTDREmethod` = "regression (nacc)", wait_ = FALSE)
  # fmt: skip
  app$set_inputs(`colSelect-REY6RECmethod` = "T-score (NA)", wait_ = FALSE)
  # fmt: skip
  app$set_inputs(`colSelect-REYDRECmethod` = "T-score (NA)", wait_ = FALSE)
  # fmt: skip
  app$set_inputs(`colSelect-LOGIMEMmethod` = "regression (nacc_legacy)", wait_ = FALSE)
  # fmt: skip
  app$set_inputs(`colSelect-MEMUNITSmethod` = "regression (nacc_legacy)", wait_ = FALSE)
  # fmt: skip
  app$set_inputs(`colSelect-REYTOTALmethod` = "T-score (NA)", wait_ = FALSE)
  # fmt: skip
  app$set_inputs(`colSelect-REYARECmethod` = "T-score (NA)", wait_ = FALSE)
  # fmt: skip
  app$set_inputs(`colSelect-TRAILBmethod` = "regression (nacc)", wait_ = FALSE)
  # fmt: skip
  app$set_inputs(`colSelect-OTRAILBmethod` = "regression (updated_2025.06)", wait_ = FALSE)
  # Update unbound `input` value
  # app$click("goToColSelect")
  app$wait_for_idle()
  app$run_js(script = "$('button[data-dismiss=\"modal\"]').click()")
  app$set_window_size(width = 1920, height = 1080)
  app$set_inputs(current_studyid = "NACC003344", wait_ = FALSE)
  # Update output value
  # Update unbound `input` value
  # Update output value
  app$wait_for_idle()
  app$expect_screenshot(
    screenshot_args = list(selector = "viewport")
  )
  app$set_inputs(current_studyid = "NACC003789")
  # Update unbound `input` value
  # Update output value
  # Update unbound `input` value
  app$set_window_size(width = 1920, height = 1080)
  # Update unbound `input` value
  app$expect_screenshot(
    screenshot_args = list(selector = "viewport")
  )
  app$set_inputs(`long-trends` = "Cognitive Scores (Table)")
  # Update output value
  app$set_window_size(width = 1920, height = 1080)
  # Update unbound `input` value
  app$expect_screenshot(
    screenshot_args = list(selector = "viewport")
  )
  # Update unbound `input` value
  app$set_window_size(width = 1920, height = 1080)
  app$set_inputs(`long-trends` = "Diagnoses")
  # Update output value
  app$expect_screenshot(
    screenshot_args = list(selector = "viewport")
  )
})
