test_that("assessment_summary_table", {
  for_test <- data.table::data.table(prepare_data(demo_data[5, ]))

  assessment_table <- assessment_summary_table(assessment_summary_data(
    for_test
  ))$`_data`

  expect_equal(
    colnames(assessment_table),
    c(
      "group",
      "labels",
      "name",
      "raw",
      "raw_suffix",
      "units",
      "std",
      "Percentile",
      "Description",
      "is_error"
    )
  )

  expect_true(is.numeric(assessment_table$Percentile))
  expect_true(is.character(assessment_table$Description))
})
