test_that("wadrc_data_prep errors", {
  expect_error(
    wadrc_data_prep(adrc_data = data.frame(), uds = "uds2"),
    regexp = "The `adrc_data` must be "
  )
})
