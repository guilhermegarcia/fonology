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
    c("pa.\u0281i", "li.\u0254\u0303*", "m\u025bt\u0281")
  )
})

test_that("French ipa() uses Lexique lookup before regex fallback", {
  expect_equal(
    ipa(c("femme", "oignon", "ch\u00e2teau", "accueil", "informatique"), lg = "fr"),
    c("fam", "\u0254.\u0272\u0254\u0303", "\u0283a.to", "a.k\u0153j", "\u025b\u0303.f\u0254\u0281.ma.tik")
  )
})

test_that("French ipa() falls back to regex for words absent from Lexique", {
  expect_false("flopranto" %in% fr_lex$word)
  expect_equal(
    ipa("flopranto", lg = "fr"),
    "flo.p\u0281\u0251\u0303.to*"
  )
})

test_that("French regex fallback marker is not treated as a segment", {
  expect_equal(
    ipa("flopranto", lg = "fr") |> getSyl(pos = 1) |> syllable(const = "coda"),
    NA_character_
  )

  expect_equal(
    syllable("mat*", const = "coda"),
    "t"
  )
})

test_that("French user IPA overrides take priority over Lexique", {
  old_lex <- .get_user_lex("fr_ipa_lex")
  on.exit(.set_user_lex("fr_ipa_lex", old_lex), add = TRUE)

  override <- "f\u0259m"
  new_lex <- c(old_lex[names(old_lex) != "femme"], femme = override)
  .set_user_lex("fr_ipa_lex", new_lex)

  expect_equal(ipa("femme", lg = "fr"), override)
})

test_that("Portuguese ipa() handles mixed valid and digit-bearing vector input", {
  expect_equal(
    ipa(c("agosto", 1869, "bella"), lg = "pt"),
    c("a.\u02c8gos.to", NA, "\u02c8be.la")
  )
})

test_that("Portuguese syllabification does not allow sm onsets", {
  expect_equal(
    ipa("transmitti", lg = "pt"),
    "trans.mi.\u02c8ti"
  )
})

test_that("Portuguese ipa() uses lexical x in known words", {
  expect_equal(
    ipa("abacaxi", lg = "pt"),
    "a.ba.ka.\u02c8\u0283i"
  )
})

test_that("Spanish loanwords keep falling au diphthongs", {
  expect_equal(
    ipa("Auckland", lg = "sp"),
    "aw.\u02c8klan.d"
  )
})

test_that("Italian loanwords keep falling au diphthongs", {
  expect_equal(
    ipa("Auckland", lg = "it"),
    "\u02c8aw.kland"
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
