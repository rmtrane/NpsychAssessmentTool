library(shinytest2)

test_that("Single REDCap source", {
  skip_on_cran()
  skip_on_ci()
  skip_if(is.null(getOption("redcap_adrc_uds3")))

  app <- AppDriver$new(
    app_dir = dataSelectApp(testing = TRUE),
    variant = platform_variant(),
    name = "single-source",
    height = 968,
    width = 1619
  )

  app$wait_for_idle(timeout = 5500)
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
  app$wait_for_idle(timeout = 3000)

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

  app$wait_for_idle(timeout = 5000)

  app$expect_values()
  app$stop()
})

test_that("Two REDCap sources", {
  skip_on_cran()
  skip_on_ci()
  skip_if(is.null(getOption("redcap_adrc_uds3")))
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
  app$wait_for_idle(timeout = 5500)

  app$set_window_size(width = 1619, height = 968)
  app$expect_screenshot()
  app$set_inputs(`dat_select-data_type` = "wadrc_uds4")
  # Update output value
  app$wait_for_idle(timeout = 3000)

  app$set_window_size(width = 1619, height = 968)
  app$expect_screenshot()
  app$set_inputs(
    `dat_select-redcap_uri` = gsub(
      pattern = "/$",
      replacement = "",
      getOption("redcap_adrc_uds4")$redcap_uri
    )
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

  app$click("dat_select-submit", wait_ = FALSE)

  app$wait_for_value(export = "dat_obj")

  dat_obj_cols <- colnames(app$get_value(export = "dat_obj"))

  expect_snapshot(dat_obj_cols)

  expect_null(app$get_value(export = "biomarker_api"))

  app$stop()
})

test_that("biomarker_api passed through", {
  skip_on_cran()
  skip_if(!panda_access)

  app <- AppDriver$new(
    app_dir = dataSelectApp(testing = TRUE),
    variant = platform_variant(),
    name = "biomarker_api",
    height = 968,
    width = 1619
  )

  app$wait_for_idle()
  app$set_window_size(width = 1619, height = 968)

  app$set_inputs(`dat_select-data_type` = "wadrc_uds4")
  # Update output value
  app$wait_for_idle(timeout = 3000)

  app$set_window_size(width = 1619, height = 968)

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

  app$set_inputs(`dat_select-data_source` = "panda")
  # Update output value
  app$wait_for_idle(timeout = 1000)
  app$set_window_size(width = 1619, height = 968)

  app$set_inputs(
    `dat_select-panda_api_token` = "secret-panda-api"
  )

  app$expect_screenshot()

  app$click("dat_select-fetch_data_button")
  app$wait_for_idle()

  app$click("dat_select-submit")

  expect_identical(app$get_value(export = "biomarker_api"), "secret-panda-api")

  app$stop()
})


test_that("save and retrieve works", {
  skip_on_cran()

  skip_if(!panda_access)

  app <- AppDriver$new(
    app_dir = dataSelectApp(testing = TRUE),
    variant = platform_variant(),
    name = "save_and_retrieve",
    height = 968,
    width = 1619,
    view = interactive()
  )

  app$set_window_size(width = 1619, height = 916)
  app$set_inputs(`dat_select-data_source` = "panda")
  # Update output value
  app$wait_for_idle()
  app$set_inputs(`dat_select-panda_api_token` = "secret-panda-api")
  # Update output value
  app$click("dat_select-fetch_data_button")
  app$wait_for_idle()
  # Update output value
  app$click("dat_select-save_data_sources")
  app$wait_for_idle()

  app$set_inputs(`dat_select-data_file_key` = "testing")
  app$set_inputs(`dat_select-data_file_key_repeated` = "testing")
  app$wait_for_idle()

  downloaded_file <- app$get_download(
    "dat_select-download_data_sources",
    filename = tempfile(fileext = ".bin")
  )

  app$run_js(
    script = "Shiny.setInputValue('dat_select-download_data_sources_clicked', 1, {priority: 'event'})"
  )

  data_obj <- safer::retrieve_object(conn = downloaded_file, key = "testing")

  data_sources <- app$get_value(export = "dat_select-data_sources")

  expect_equal(
    data_obj,
    data_sources
  )

  app$set_inputs(`dat_select-data_source` = "retrieve")
  # Update output value
  # Uploaded file outside of: ./tests/testthat
  app$upload_file(`dat_select-saved_data_file` = downloaded_file)
  app$set_inputs(`dat_select-data_file_key` = "testing")
  app$wait_for_idle()
  # Update output value
  app$click("dat_select-retrieve_data_button")

  app$wait_for_idle()

  expect_equal(
    app$get_value(export = "dat_select-data_sources"),
    list("1" = data_sources[[1]], "2" = data_sources[[1]])
  )

  app$stop()
})

test_that("Works when packages not installed", {
  local_mocked_bindings(
    is_installed = function(pkg, quietly = TRUE) {
      FALSE
      # if (pkg %in% c("REDCapR")) {
      #   return(FALSE)
      # }
      # rlang::is_installed(pkg)
    },
    .package = "rlang"
  )

  app <- AppDriver$new(
    app_dir = dataSelectApp(testing = TRUE),
    variant = platform_variant(),
    name = "pkgs_disabled",
    height = 968,
    width = 1619
  )

  app$set_window_size(width = 1619, height = 916)

  app$run_js(script = "$('.selectize-input').first().click()")

  app$expect_screenshot(threshold = 7)

  app$stop()
})
