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

test_that("syllable() treats schwa as a nucleus", {
  phon <- c("d\u0279\u026am", "d\u0259", "\u0279i")

  expect_equal(
    phon |> getSyl(pos = 1) |> syllable(const = "onset"),
    c("d\u0279", "d", "\u0279")
  )

  expect_equal(
    phon |> getSyl(pos = 1) |> syllable(const = "nucleus"),
    c("\u026a", "\u0259", "i")
  )

  expect_equal(
    phon |> getSyl(pos = 1) |> syllable(const = "coda"),
    c("m", NA, NA)
  )
})
