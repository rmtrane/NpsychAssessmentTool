test_that("get_y_range", {
  tmp_dat <- data.table::data.table(
    x1 = c(-5, 5),
    x2 = c(-2, 3),
    x3 = c(-3, 1),
    x4 = c(-1, 0.5),
    x5 = c(NA, NA)
  )

  expect_error(get_y_range(data.frame(tmp_dat)))

  expect_equal(
    get_y_range(tmp_dat),
    c(-5, 5)
  )

  expect_equal(
    get_y_range(tmp_dat[, list(x2, x3)]),
    c(-3, 3)
  )

  expect_equal(
    get_y_range(tmp_dat[, list(x4)]),
    c(-2.5, 2.5)
  )
})
