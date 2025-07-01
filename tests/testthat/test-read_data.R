test_that("read_data works as expected", {
  ## Test that demo_data is returned
  expect_equal(
    read_data(
      data_source = "demo"
    ),
    demo_data
  )

  ############
  # Check correct errors are given
  expect_error(
    read_data(data_source = "redcap"),
    regexp = "`redcap_auth` must be provided when `data_source` is \"redcap\""
  )

  expect_error(
    read_data(
      data_source = "redcap",
      redcap_auth = list(a = 1)
    ),
    regexp = "`redcap_auth` must have entries named \"redcap_uri\" and \"token\"."
  )

  expect_error(
    read_data(
      data_source = "redcap",
      redcap_auth = list(redcap_uri = 1, token = 1),
      data_type = "something"
    ),
    regexp = "`data_type` must be \"wadrc_uds3\" or \"wadrc_uds4\" when `data_source` is \"redcap\"."
  )

  expect_warning(
    read_data(
      data_source = "demo",
      data_type = "something"
    ),
    regexp = "`data_type` is ignored"
  )
})
