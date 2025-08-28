test_that("return_single", {
  expect_equal(
    return_single(c(NA, 1, 1)),
    1
  )

  expect_equal(
    return_single(c(NA, 1, NA)),
    1
  )

  expect_equal(
    return_single(c(NA, 1, 2)),
    NA
  )

  expect_equal(
    return_single(c(NA, NA)),
    NA
  )

  expect_equal(
    return_single(c(1, 2), if_multiple = "A"),
    "A"
  )
})
