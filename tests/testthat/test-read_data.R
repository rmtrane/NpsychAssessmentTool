test_that("read_data works as expected", {
  skip_on_ci()

  ############
  # Check correct errors are given
  expect_error(
    read_data(
      data_source = "redcap",
      redcap_auth = list(
        redcap_uri = "testing/API/",
        token = paste(
          sample(c(0:9, LETTERS[1:6], letters[1:6]), 32, replace = T),
          collapse = ""
        )
      ),
      data_type = "wadrc_uds3"
    ),
    regexp = "Unable to access REDCap."
  )

  expect_error(
    read_data(data_source = "redcap"),
    regexp = "must be provided when"
  )

  expect_error(
    read_data(data_source = "redcap", redcap_auth = "not-a-list"),
    regexp = "redcap_auth"
  )

  expect_error(
    read_data(
      data_source = "redcap",
      redcap_auth = list(redcap_uri = NULL, token = "token")
    ),
    regexp = "must be strings, not NULL"
  )

  expect_error(
    read_data(
      data_source = "redcap",
      redcap_auth = list(a = 1)
    ),
    regexp = "must have entries named"
  )

  expect_error(
    read_data(
      data_source = "redcap",
      redcap_auth = list(redcap_uri = 1, token = 1),
      data_type = "something"
    )
  )

  expect_error(
    read_data(
      data_source = "redcap",
      redcap_auth = list(redcap_uri = 1, token = 1),
      data_type = "wadrc_uds3"
    ),
    regexp = "Unable to access REDCap."
  )

  expect_warning(
    read_data(
      data_source = "demo",
      data_type = "something"
    ),
    regexp = "`data_type` is ignored"
  )

  ############
  # Test that demo_data is returned
  expect_equal(
    read_data(
      data_source = "demo"
    ),
    demo_data
  )

  # Test that this works for .csv file. Save demo_data to .csv
  tmp_filename <- tempfile(fileext = ".csv")

  write.csv(demo_data, file = tmp_filename, row.names = FALSE)

  from_csv <- read_data(
    data_source = "csv_upload",
    data_file = tmp_filename
  )

  expect_equal(
    from_csv |> dplyr::mutate(COGOTH3X = ifelse(is.na(COGOTH3X), "", COGOTH3X)),
    demo_data # |> dplyr::mutate(dplyr::across(tidyselect::everything(), \(x) ifelse(all(x == ""), NA, x)))
  )

  file.remove(tmp_filename)

  # Test reading from REDCap
  skip_if(is.null(getOption("redcap_adrc_uds4")))

  redcap_uds4_data <- REDCapR::redcap_read_oneshot(
    redcap_uri = getOption("redcap_adrc_uds4")$redcap_uri,
    token = getOption("redcap_adrc_uds4")$token,
    fields = wadrc_uds4_redcap_fields,
    guess_max = Inf
  )$data |>
    dplyr::filter(
      !grepl("uds3", redcap_event_name)
    ) |>
    data.table::as.data.table()

  expect_equal(
    read_data(
      data_source = "redcap",
      data_type = "wadrc_uds4",
      redcap_auth = getOption("redcap_adrc_uds4")
    ),
    wadrc_data_prep(
      adrc_data = redcap_uds4_data,
      uds = "uds4"
    )
  )

  expect_warning(
    read_data(
      data_source = "redcap",
      data_type = "wadrc_uds4",
      redcap_auth = getOption("redcap_adrc_uds4"),
      data_file = "test.csv"
    )
  )

  uds4_csv <- tempfile(fileext = ".csv")

  write.csv(redcap_uds4_data, file = uds4_csv)

  expect_equal(
    object = read_data(
      data_source = "csv_upload",
      data_type = "wadrc_uds4",
      data_file = uds4_csv
    ) |>
      unique(),
    expected = wadrc_data_prep(
      adrc_data = redcap_uds4_data,
      uds = "uds4"
    ) |>
      unique()
  )

  file.remove(uds4_csv)
})
