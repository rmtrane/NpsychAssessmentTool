test_that("read_data works as expected", {
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

  # Test that this works for .csv file. Run locally, so we can use NACC data
  nacc_data_path <- "~/Documents/NACC-data/data/investigator_nacc66.csv"

  skip_if_not(
    file.exists(nacc_data_path),
    message = "NACC data not found. Skipping."
  )

  nacc_data <- data.table::fread(
    file = nacc_data_path,
    na.strings = c("", "NA")
  )

  nacc_data_2 <- read_data(
    data_source = "csv_upload",
    data_file = "~/Documents/NACC-data/data/investigator_nacc66.csv"
  )

  expect_equal(
    nacc_data,
    nacc_data_2
  )

  # Test reading from REDCap
  skip_if(is.null(getOption("redcap_adrc_uds4")))

  suppressMessages(
    redcap_uds4_data <- REDCapR::redcap_read_oneshot(
      redcap_uri = getOption("redcap_adrc_uds4")$redcap_uri,
      token = getOption("redcap_adrc_uds4")$token,
      fields = wadrc_uds4_redcap_fields,
      guess_max = Inf
    )$data |>
      data.table::as.data.table() |>
      wadrc_data_prep(
        uds = "uds4"
      )
  )

  expect_equal(
    suppressWarnings(
      classes = c("warning", "message"),
      read_data(
        data_source = "redcap",
        data_type = "wadrc_uds4",
        redcap_auth = getOption("redcap_adrc_uds4"),
        data_file = "test.csv"
      )
    ),
    redcap_uds4_data
  )

  expect_warning(
    read_data(
      data_source = "redcap",
      data_type = "wadrc_uds4",
      redcap_auth = getOption("redcap_adrc_uds4"),
      data_file = "test.csv"
    )
  )
})
