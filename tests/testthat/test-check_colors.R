test_that("check_colors works as expected", {
  # fmt: skip
  hex_colors <- paste0(
    "#",
    c("ABF6C9", "14FBD9", "AF6286", "C4A39B", "D48A50", "D4A7E4", "31D692", "DFAD51", "889310", "3C1E5E", "26E7F5", "F66672", "18D92F", "70EE04", "B417A6", "A83538", "026E2C", "C2861E", "0E58F0", "55A6AF", "37FC99", "2CB6E2", "3B56B7", "A01158", "18024B", "488C90", "4EAF9E", "8E86EB", "BE93C7", "50E6A1", "E83885", "C352CB", "86E931", "EFA97D", "E76B9A", "BA3E63", "DA50BB", "560ED2", "03B43D", "74EF45", "B6CD51", "133788", "C269AD", "3AF5CE", "103E57", "41492B", "D605F8", "1A0837", "9A20AE", "7DB885", "276FF0", "C682EA", "5F4822", "94C3E1", "2DBC38", "3F5297", "7DD1DB", "A00168", "30B1B3", "0AFE83", "096404", "2A01D2", "47274B", "8A7D9F", "6E7F94", "45EECE", "FE902D", "846EA6", "FF6AC1", "99ECAF", "57E0AC", "8C96D4", "01F730", "B44296", "0FEDDC", "BB956E", "17A05A", "88DF2F", "6A4BE8", "107AB3", "982BDF", "DF1045", "5A19EF", "18FB3A", "4A5F9C", "EF2B15", "BFB354", "C989A1", "B61407", "DDCD20", "2A7158", "F2E030", "1A0BB2", "D0F0CD", "BD59C9", "C656A6", "94655C", "84E991", "A9D7B0", "3AD094")
  )

  expect_true(check_colors(hex_colors))
  expect_length(check_colors(hex_colors, return_non_colors = T), 0L)

  not_hex_colors <- c("red", "something", "124")

  expect_false(check_colors(not_hex_colors))
  expect_length(
    check_colors(not_hex_colors, return_non_colors = T),
    length(not_hex_colors)
  )

  expect_equal(
    check_colors(not_hex_colors, return_non_colors = T),
    setNames(not_hex_colors, 1:3)
  )

  # Test with a mix of valid hex colors and non-hex colors
  expect_false(check_colors(c(hex_colors, not_hex_colors)))

  mix <- sample(c(hex_colors, not_hex_colors), 103, replace = FALSE)
  non_hex <- which(mix %in% not_hex_colors)

  expect_length(check_colors(mix, return_non_colors = T), length(non_hex))
  expect_equal(
    check_colors(mix, return_non_colors = T)[as.character(sort(non_hex))],
    setNames(mix[non_hex], non_hex)
  )
})
