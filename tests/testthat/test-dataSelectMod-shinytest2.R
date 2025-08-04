library(shinytest2)

test_that("dataSelectModule works", {
  skip_on_cran()
  skip_if(is.null(getOption("redcap_adrc_uds3")))

  app <- AppDriver$new(
    app_dir = dataSelectApp(testing = TRUE),
    variant = platform_variant(),
    name = "single-source",
    height = 968,
    width = 1619
  )

  app$wait_for_idle()
  app$set_window_size(width = 1619, height = 968)
  app$expect_screenshot()

  # Update output value

  app$set_inputs(`dat_select-data_type` = "wadrc_uds3")
  # Update output value
  app$wait_for_idle(timeout = 1000)
  app$expect_screenshot()
  app$set_inputs(
    `dat_select-redcap_uri` = getOption("redcap_adrc_uds3")$redcap_uri
  )
  app$set_inputs(`dat_select-api_token` = getOption("redcap_adrc_uds3")$token)
  # Update output value
  app$wait_for_idle()

  app$set_window_size(width = 1619, height = 968)
  app$expect_screenshot()
  app$set_inputs(
    `dat_select-fetch_data_button` = "click",
    wait_ = FALSE
  )
  app$wait_for_idle()
  app$set_window_size(width = 1619, height = 968)
  app$expect_screenshot()
  # Update output value

  app$click("dat_select-submit", timeout_ = 20000)

  app$wait_for_idle(timeout = 10000)

  app$expect_values()
  app$stop()

  skip_if(is.null(getOption("redcap_adrc_uds4")))

  app <- AppDriver$new(
    app_dir = dataSelectApp(testing = TRUE),
    variant = platform_variant(),
    name = "dual-source",
    height = 968,
    width = 1619
  )

  app$wait_for_idle()
  app$set_window_size(width = 1619, height = 968)
  app$expect_screenshot()

  # Update output value
  app$set_inputs(`dat_select-data_type` = "wadrc_uds3")
  # Update output value
  app$wait_for_idle(timeout = 1000)
  app$set_window_size(width = 1619, height = 968)
  app$expect_screenshot()
  app$set_inputs(
    `dat_select-redcap_uri` = getOption("redcap_adrc_uds3")$redcap_uri
  )
  app$set_inputs(`dat_select-api_token` = getOption("redcap_adrc_uds3")$token)
  # Update output value
  app$wait_for_idle()

  app$set_window_size(width = 1619, height = 968)
  app$expect_screenshot()
  app$set_inputs(
    `dat_select-fetch_data_button` = "click",
    wait_ = FALSE
  )
  app$wait_for_idle()

  app$set_window_size(width = 1619, height = 968)
  app$expect_screenshot()
  app$set_inputs(`dat_select-data_type` = "wadrc_uds4")
  # Update output value
  app$wait_for_idle(timeout = 1000)

  app$set_window_size(width = 1619, height = 968)
  app$expect_screenshot()
  app$set_inputs(
    `dat_select-redcap_uri` = getOption("redcap_adrc_uds4")$redcap_uri
  )
  app$set_inputs(`dat_select-api_token` = getOption("redcap_adrc_uds4")$token)
  # Update output value
  app$wait_for_idle()

  app$set_window_size(width = 1619, height = 968)
  app$expect_screenshot()
  app$set_inputs(
    `dat_select-fetch_data_button` = "click",
    wait_ = FALSE
  )
  app$wait_for_idle()

  app$set_window_size(width = 1619, height = 968)
  app$expect_screenshot()
  # Update output value

  app$click("dat_select-submit", timeout_ = 20000)

  app$wait_for_idle(timeout = 10000)

  app$expect_values()
  app$stop()
})
