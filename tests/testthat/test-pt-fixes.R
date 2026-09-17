# Bug fixes in the Portuguese transcriber (Fonology 1.7.1).

test_that("no orthographic x survives into a transcription", {
  # Nonce words force the regex fallback. x used to surface verbatim when two
  # intervocalic x's were adjacent (the first match consumed the vowel the
  # second needed), before a nasal vowel, after a consonant, or before a
  # consonant not listed in the rule.
  expect_equal(ipa("zaxixe", lg = "pt"), "za.\u02c8\u0283i.\u0283e*")
  expect_equal(ipa("zorxe", lg = "pt"), "\u02c8z\u0254\u027e.\u0283e*")
  expect_equal(ipa("zexfo", lg = "pt"), "\u02c8zes.fo*")
  expect_equal(ipa("zaxwe", lg = "pt"), "\u02c8zak.swe*")
  expect_equal(ipa("conex\u00e3o", lg = "pt"), "ko.ne.\u02c8\u0283\u00e3w\u0303*")

  words <- c("zaxixe", "zorxe", "zexfo", "zaxwe", "zaxl", "exchange", "axl", "conex\u00e3o", "lix\u00e3o")
  expect_false(any(stringr::str_detect(ipa(words, lg = "pt"), "x")))
})

test_that("spelled \u00e3i is the nasal diphthong, like \u00e3e", {
  expect_equal(ipa("z\u00e3ibra", lg = "pt"), "\u02c8z\u00e3j\u0303.b\u027ea*")
})

test_that("\u00f1 is the palatal nasal", {
  expect_equal(ipa("ze\u00f1or", lg = "pt"), "ze.\u02c8\u0272o\u027e*")
})

test_that("ipa() removes hyphenated enclitics", {
  # strip_clitic_pt() ran after punctuation had already removed the hyphen.
  expect_equal(ipa("diz-me", lg = "pt"), "\u02c8dis*")
  expect_equal(ipa("v\u00ea-los", lg = "pt"), "\u02c8ve*")
  # Hyphenated compounds are not clitic hosts.
  expect_equal(ipa("guarda-chuva", lg = "pt"), "gwa\u027e.da.\u02c8\u0283u.va*")
})

test_that("biGram_pt() accepts glides", {
  # w was on the list of orthographic letters, so any input with /w/ was
  # rejected as spelling.
  expect_true(is.finite(biGram_pt("\u02c8n\u00e3w\u0303")))
  expect_true(is.finite(biGram_pt("\u02c8kwa.d\u027eo")))
})

test_that("narrow e + nasal keeps the nasal and diphthongises only word-finally", {
  # The rule used to drop the nasal consonant and insert \u0272 everywhere.
  expect_equal(ipa("tempo", lg = "pt", narrow = TRUE), "\u02c8te\u0303m.p\u028a")
  expect_equal(ipa("genro", lg = "pt", narrow = TRUE), "\u02c8\u0292e\u0303n.x\u028a")
  expect_equal(ipa("homens", lg = "pt", narrow = TRUE), "\u02c8o.me\u0303j\u0303s")
  expect_false(any(stringr::str_detect(
    ipa(c("tempo", "entrada", "sempre", "bem", "homens"), lg = "pt", narrow = TRUE),
    "j\u0303\u0272"
  )))
})

test_that("transcribe_pt() is the maintained transcriber", {
  # It used to be a scalar copy that had drifted: no phonemic rhotics and some
  # nasal diphthongs unconverted.
  expect_equal(transcribe_pt("carro"), "karo")
  expect_equal(transcribe_pt("caro"), "ka\u027eo")
  expect_equal(transcribe_pt("lim\u00e3oranazinho"), "lim\u00e3w\u0303\u027eanazi\u0272o")
})

test_that("psl stemPro and proU follow their definitions", {
  psl <- Fonology:::.get_pkg_data("psl")
  proU <- as.character(psl$proU)
  expect_false(any(grepl("[-']", proU)))
  expect_true(all(as.character(psl$stemPro) == substr(proU, 1, nchar(proU) - 1)))
  # A nasal-final stem drops the nasal coda, its final segment.
  expect_equal(as.character(psl$stemPro[psl$word == "abi\u00e3"][1]), "abia")
})
