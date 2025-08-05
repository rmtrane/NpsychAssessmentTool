test_that("error", {
  expect_error(
    prev_diagnoses_table(dat = data.frame()),
    regexp = "object must be a"
  )
})

test_that("prev_diagnoses_table works", {
  prep_demo <- prepare_data(
    demo_data,
    selected_cols = colnames(demo_data),
    print_messages = F,
    with_diags = T
  )

  no_diagnoses <- prep_demo |>
    subset(NACCID == "NACC097067") |>
    transform(
      NACCUDSD = NA_integer_
    )

  tmp_tab <- prev_diagnoses_table(
    dat = prep_demo |> subset(NACCID == "NACC097067")
  )

  expect_snapshot(tmp_tab)
})
