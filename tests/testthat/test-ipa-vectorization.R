test_that("Spanish ipa() remains vectorized", {
  expect_equal(
    ipa(c("Madrid", "Granada", "Barcelona"), lg = "sp"),
    c("ma.\u02c8d\u027eid", "g\u027ea.\u02c8na.da", "ba\u027e.se.\u02c8lo.na")
  )
})

test_that("Spanish ipa() handles mixed valid and digit-bearing vector input", {
  expect_equal(
    ipa(c("Madrid", "abc123", "Granada"), lg = "sp"),
    c("ma.\u02c8d\u027eid", NA, "g\u027ea.\u02c8na.da")
  )
})

test_that("French ipa() remains vectorized", {
  expect_equal(
    ipa(c("Paris", "Lyon", "mettre"), lg = "fr"),
    c("pa.\u0281i", "li.\u0254\u0303", "m\u025bt\u0281")
  )
})
