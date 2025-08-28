test_that("demographics_table", {
  tmp_dat_in <- data.table::data.table(
    NACCID = "ptid",
    EDUC = 15,
    BIRTHYR = 1959,
    SEX = 2,
    HANDED = 2,
    RACE = 2,
    somescore = c(1, 2, 3, 4)
  )

  tmp_dat_out <- tmp_dat_in |>
    dplyr::select(-somescore) |>
    unique() |>
    dplyr::mutate(
      across(-c(NACCID, EDUC, BIRTHYR), \(x) {
        NpsychBatteryNorms::values_to_labels(x, dplyr::cur_column())
      })
    ) |>
    tidyr::pivot_longer(cols = everything(), values_transform = as.character) |>
    dplyr::mutate(
      name = dplyr::case_match(
        name,
        "NACCID" ~ "Study ID:",
        "EDUC" ~ "Education (years):",
        "BIRTHYR" ~ "Year of Birth:",
        "SEX" ~ "Gender:",
        "HANDED" ~ "Handedness:",
        "RACE" ~ "Race:"
      ) |>
        factor(
          levels = c(
            "Study ID:",
            "Education (years):",
            "Year of Birth:",
            "Gender:",
            "Handedness:",
            "Race:"
          )
        )
    )

  tmp_dem_tab <- demographics_table(tmp_dat_in)

  expect_equal(
    tmp_dem_tab$`_data`,
    tmp_dat_out
  )

  expect_snapshot(gt::as_raw_html(tmp_dem_tab))

  tmp_dat_in <- tmp_dat_in |>
    dplyr::mutate(
      EDUC = c(13:16)
    )

  tmp_dat_out <- tmp_dat_in |>
    dplyr::select(-somescore) |>
    unique() |>
    dplyr::mutate(
      EDUC = "13/14/15/16",
      across(-c(NACCID, EDUC, BIRTHYR), \(x) {
        NpsychBatteryNorms::values_to_labels(x, dplyr::cur_column())
      })
    ) |>
    unique() |>
    tidyr::pivot_longer(cols = everything(), values_transform = as.character) |>
    dplyr::mutate(
      name = dplyr::case_match(
        name,
        "NACCID" ~ "Study ID:",
        "EDUC" ~ "Education (years):",
        "BIRTHYR" ~ "Year of Birth:",
        "SEX" ~ "Gender:",
        "HANDED" ~ "Handedness:",
        "RACE" ~ "Race:"
      ) |>
        factor(
          levels = c(
            "Study ID:",
            "Education (years):",
            "Year of Birth:",
            "Gender:",
            "Handedness:",
            "Race:"
          )
        )
    )

  tmp_dem_tab <- demographics_table(tmp_dat_in)

  expect_equal(
    tmp_dem_tab$`_data`,
    tmp_dat_out
  )

  expect_snapshot(gt::as_raw_html(tmp_dem_tab))

  expect_error(
    demographics_table(
      tmp_dat_in |> dplyr::mutate(NACCID = paste0("ptid", 1:4))
    ),
    regexp = "There should only be one study ID in data, not 4"
  )

  expect_error(
    demographics_table(data.frame(tmp_dat_in)),
    regexp = "The `dat` argument must be a"
  )
})
