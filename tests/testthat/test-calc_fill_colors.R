test_that("calc_fill_colors", {
  expect_error(calc_fill_colors())
  expect_error(
    calc_fill_colors(16),
    regexp = "n must be at most 11"
  )

  expect_error(
    calc_fill_colors(2),
    regexp = "n must be at least 3"
  )

  targets <- list(
    "3" = grDevices::rgb(
      c(252, 255, 145),
      c(141, 255, 207),
      c(89, 191, 96),
      maxColorValue = 255
    ),
    "4" = grDevices::rgb(
      c(215, 253, 166, 26),
      c(25, 174, 217, 150),
      c(28, 97, 106, 65),
      maxColorValue = 255
    ),
    "5" = grDevices::rgb(
      c(215, 253, 255, 166, 26),
      c(25, 174, 255, 217, 150),
      c(28, 97, 191, 106, 65),
      maxColorValue = 255
    ),
    "6" = grDevices::rgb(
      c(215, 252, 254, 217, 145, 26),
      c(48, 141, 224, 239, 207, 152),
      c(39, 89, 139, 139, 96, 80),
      maxColorValue = 255
    ),
    "7" = grDevices::rgb(
      c(215, 252, 254, 255, 217, 145, 26),
      c(48, 141, 224, 255, 239, 207, 152),
      c(39, 89, 139, 191, 139, 96, 80),
      maxColorValue = 255
    ),
    "8" = grDevices::rgb(
      c(215, 244, 253, 254, 217, 166, 102, 26),
      c(48, 109, 174, 224, 239, 217, 189, 152),
      c(39, 67, 97, 139, 139, 106, 99, 80),
      maxColorValue = 255
    ),
    "9" = grDevices::rgb(
      c(215, 244, 253, 254, 255, 217, 166, 102, 26),
      c(48, 109, 174, 224, 255, 239, 217, 189, 152),
      c(39, 67, 97, 139, 191, 139, 106, 99, 80),
      maxColorValue = 255
    ),
    "10" = grDevices::rgb(
      c(165, 215, 244, 253, 254, 217, 166, 102, 26, 0),
      c(0, 48, 109, 174, 224, 239, 217, 189, 152, 104),
      c(38, 39, 67, 97, 139, 139, 106, 99, 80, 55),
      maxColorValue = 255
    ),
    "11" = grDevices::rgb(
      c(165, 215, 244, 253, 254, 255, 217, 166, 102, 26, 0),
      c(0, 48, 109, 174, 224, 255, 239, 217, 189, 152, 104),
      c(38, 39, 67, 97, 139, 191, 139, 106, 99, 80, 55),
      maxColorValue = 255
    )
  )

  for (n in 3:11) {
    cur_cols <- calc_fill_colors(n)
    expect_true(is.character(cur_cols))
    expect_equal(length(cur_cols), n)
    expect_equal(cur_cols, targets[[as.character(n)]])
  }
})
