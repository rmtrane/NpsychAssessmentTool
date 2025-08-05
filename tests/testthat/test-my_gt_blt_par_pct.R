test_that("my-gt-blt-bar-pct", {
  mdash <- "—"

  ## Some missing
  tbl <- data.frame(x = 1:5, y = c(NA, 2:5 / 6))

  ## Test without labels
  gt_tbl <- gt::gt(tbl, id = "tmp") |>
    my_gt_plt_bar_pct(column = "y") |>
    gt::as_raw_html()

  gt_tbl |>
    rvest::read_html() |>
    rvest::html_table() |>
    _[[1]] |>
    as.data.frame() |>
    expect_equal(
      tbl |>
        transform(
          y = ifelse(
            is.na(y),
            mdash,
            ""
          )
        )
    )

  expect_snapshot(gt_tbl)

  ## Test with labels
  gt_tbl <- gt::gt(tbl, id = "tmp") |>
    my_gt_plt_bar_pct(
      column = "y",
      labels = T
    ) |>
    gt::as_raw_html()

  gt_tbl |>
    rvest::read_html() |>
    rvest::html_table() |>
    _[[1]] |>
    as.data.frame() |>
    expect_equal(
      tbl |>
        transform(
          y = ifelse(
            is.na(y),
            mdash,
            paste0(round(y / max(y, na.rm = T) * 100), "%")
          )
        )
    )

  expect_snapshot(gt_tbl)

  # Test with scaled = TRUE
  gt_tbl <- gt::gt(tbl, id = "tmp") |>
    my_gt_plt_bar_pct(
      column = "y",
      labels = T,
      scaled = T
    ) |>
    gt::as_raw_html()

  gt_tbl |>
    rvest::read_html() |>
    rvest::html_table() |>
    _[[1]] |>
    as.data.frame() |>
    expect_equal(
      tbl |>
        transform(
          y = ifelse(
            is.na(y),
            mdash,
            paste0(round(y, digits = 1), "%")
          )
        )
    )

  expect_snapshot(gt_tbl)

  # Test with smaller label_cutoff
  gt_tbl <- gt::gt(tbl, id = "tmp") |>
    my_gt_plt_bar_pct(
      column = "y",
      labels = T,
      label_cutoff = 0.1
    ) |>
    gt::as_raw_html()

  gt_tbl |>
    rvest::read_html() |>
    rvest::html_table() |>
    _[[1]] |>
    as.data.frame() |>
    expect_equal(
      tbl |>
        transform(
          y = ifelse(
            is.na(y),
            mdash,
            paste0(y / max(y, na.rm = T) * 100, "%")
          )
        )
    )

  testthat::expect_snapshot(gt_tbl)

  ## All missing
  tbl <- data.frame(x = 1:5, y = NA)

  from_html_table <- gt::gt(tbl, id = "tmp") |>
    my_gt_plt_bar_pct(column = "y") |>
    gt::as_raw_html() |>
    rvest::read_html() |>
    rvest::html_table() |>
    _[[1]]

  expect_equal(
    from_html_table$y,
    c("", "—")[is.na(tbl$y) + 1]
  )
})
