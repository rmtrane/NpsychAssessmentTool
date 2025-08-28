test_that("prepare_data", {
  prep_dat <- prepare_data(demo_data, selected_cols = NULL)

  ## Test dimensions are as expected
  expect_equal(nrow(prep_dat), nrow(demo_data))
  expect_equal(
    ncol(prep_dat),
    ncol(demo_data) +
      sum(grepl("^std_", names(prep_dat))) +
      length(c("FAS", "MOCACLOCK", "REYTOTAL", "REYAREC", "VISITDATE"))
  )

  ## Test that NACCAGE is correctly calculated if not present
  demo_data_no_age <- demo_data
  demo_data_no_age$NACCAGE <- NULL

  expect_equal(
    floor(prepare_data(demo_data_no_age, selected_cols = NULL)$NACCAGE),
    demo_data$NACCAGE
  )

  ## Test that SEX is correctly converted to numeric if not already
  demo_data_sex_char <- demo_data |>
    dplyr::mutate(
      SEX = NpsychBatteryNorms::values_to_labels(SEX, "SEX")
    )

  expect_equal(
    prepare_data(demo_data_sex_char, selected_cols = NULL)$SEX,
    demo_data$SEX
  )

  demo_data_sex_char <- demo_data |>
    dplyr::mutate(
      SEX = substr(NpsychBatteryNorms::values_to_labels(SEX, "SEX"), 1, 1)
    )

  expect_equal(
    prepare_data(demo_data_sex_char, selected_cols = NULL)$SEX,
    demo_data$SEX
  )

  ## Test that non-numeric raw scores are converted to numeric correctly.
  demo_data_non_numeric <- demo_data |>
    dplyr::mutate(
      TRAILA = as.character(TRAILA)
    )

  expect_equal(
    suppressMessages(
      prepare_data(demo_data_non_numeric, selected_cols = NULL)$raw_TRAILA
    ),
    demo_data$TRAILA
  )

  expect_equal(
    suppressMessages(
      prepare_data(demo_data_non_numeric, selected_cols = NULL)$std_TRAILA
    ),
    prep_dat$std_TRAILA
  )

  expect_message(
    prepare_data(demo_data_non_numeric, selected_cols = NULL),
    regexp = "'raw_scores' for TRAILA must be a numeric vector. You provided a character, but it was successfully converted to numeric."
  )

  ## Test that function errors if raw scores cannot be converted to numeric
  expect_error(
    prepare_data(
      demo_data_non_numeric |>
        dplyr::mutate(TRAILA = "not-a-number"),
      selected_cols = NULL
    ),
    regexp = "'raw_scores' for TRAILA must be a numeric vector. You provided a vector of class character, which contains some non-digit characters, and therefore could not be converted."
  )
})
