# Portuguese rhotics (Fonology 1.7.1).
#
# Broad transcription writes the two contrastive rhotic phonemes: the strong
# rhotic as /r/ and the tap as /\u027e/. Narrow transcription realises the strong
# rhotic as [x]. Up to 1.7.0 broad transcription wrote the strong rhotic with
# its [x] allophone and the tap as r.
#
# The change was verified against 1.7.0 on 167,596 words: rewriting the new
# broad output back into the old convention reproduces the old output exactly,
# except for four lookup entries with documented data corrections, and narrow
# output is byte-identical apart from the same four.

test_that("the strong rhotic is /r/ in every strong position (lookup)", {
  expect_equal(
    ipa(c("rato", "carro", "honra"), lg = "pt"),
    c("\u02c8ra.to", "\u02c8ka.ro", "\u02c8on.ra")
  )
})

test_that("the tap is /\u027e/ intervocalically, in clusters and in coda (lookup)", {
  expect_equal(
    ipa(c("caro", "prato", "porta", "quero"), lg = "pt"),
    c("\u02c8ka.\u027eo", "\u02c8p\u027ea.to", "\u02c8p\u0254\u027e.ta", "\u02c8k\u025b.\u027eo")
  )
})

test_that("the regex fallback follows the same convention", {
  expect_equal(
    ipa(c("ranipo", "honrila", "dobrenho", "gostrar"), lg = "pt"),
    c("ra.\u02c8ni.po*", "on.\u02c8ri.la*", "do.\u02c8b\u027ee.\u0272o*", "gos.\u02c8t\u027ea\u027e*")
  )
})

test_that("narrow transcription realises the strong rhotic as [x]", {
  expect_equal(
    ipa(c("rato", "carro", "caro"), lg = "pt", narrow = TRUE),
    c("\u02c8xa.t\u028a", "\u02c8ka.x\u028a", "\u02c8ka.\u027e\u028a")
  )
})

test_that("an orthographic x never becomes a rhotic", {
  # Orthographic x is unpredictable in Portuguese (sh, ks, s, z). The rhotic
  # change must not touch it, including the forms the grapheme rules miss.
  words <- c("conex\u00e3o", "t\u00e1xi", "abacaxi", "exame", "fixo", "m\u00e1ximo")
  out <- ipa(words, lg = "pt")
  expect_false(any(stringr::str_detect(out, "[r\u027e]")))
})

test_that("the internal placeholder never reaches the output", {
  words <- c("rato", "carro", "honra", "ranipo", "honrila", "zarro", "Israel")
  expect_false(any(stringr::str_detect(
    c(ipa(words, lg = "pt"), ipa(words, lg = "pt", narrow = TRUE)), "\u0280"
  )))
})

test_that("gen_pt() uses phonemic rhotics", {
  set.seed(123)
  words <- replicate(200, Fonology:::gen_pt("LLL"))
  expect_false(any(stringr::str_detect(words, "[x\u0280]")))
  expect_true(any(stringr::str_detect(words, "r")))
})

test_that("psl_to_ipa() maps the PSL notation", {
  f <- Fonology:::psl_to_ipa
  expect_equal(f("a-ba-i-'Ra-do"), "a.ba.i.\u02c8ra.do")
  expect_equal(f("'ka-ro"), "\u02c8ka.\u027eo")
  expect_equal(f("a-ba-'da~w"), "a.ba.\u02c8d\u00e3w\u0303")
  expect_equal(f("'ka~j-bra"), "\u02c8k\u00e3j\u0303.b\u027ea")
  expect_equal(f("a-ba-'La~"), "a.ba.\u02c8\u028e\u00e3")
  expect_equal(f("'SE-ZO-No"), "\u02c8\u0283\u025b.\u0292\u0254.\u0272o")
  # Coda columns: a bare ~ is the nasal archiphoneme.
  expect_equal(f("~"), "N")
  expect_equal(f(c("None", NA)), c("None", NA))
  # The three PSL entries with an orthographic x standing for /\u0283/.
  expect_equal(f("'xe-ni-ko"), "\u02c8\u0283e.ni.ko")
  # Legacy mode reproduces the 1.7.0 convention.
  expect_equal(f("a-ba-i-'Ra-do", rhotics = "legacy"), "a.ba.i.\u02c8xa.do")
  expect_equal(f("'ka-ro", rhotics = "legacy"), "\u02c8ka.ro")
})

test_that("shipped Portuguese data use phonemic rhotics", {
  pt_lex <- Fonology:::.get_pkg_data("pt_lex")
  expect_false(any(stringr::str_detect(pt_lex$pro, "[x\u0280]")))
  expect_true(any(stringr::str_detect(pt_lex$pro, "\u027e")))

  bigrams <- Fonology:::.get_pkg_data("bigrams_pt")
  expect_false(any(stringr::str_detect(bigrams$ngrams, "x")))
  expect_equal(sum(bigrams$prop), 1)

  freq <- Fonology:::.get_pkg_data("pt_freq")
  no_ortho_x <- !grepl("x", freq$word, fixed = TRUE)
  expect_false(any(stringr::str_detect(freq$ipa[no_ortho_x], "x"), na.rm = TRUE))
})

test_that("psl keeps its notation and gains IPA columns", {
  psl <- Fonology:::.get_pkg_data("psl")
  ipa_cols <- grep("\\.ipa$", names(psl), value = TRUE)
  expect_length(ipa_cols, 21)
  expect_identical(names(psl)[1:62], sub("\\.ipa$", "", names(psl)[1:62]))
  expect_true("R" %in% unlist(strsplit(as.character(psl$pro[1:5000]), "")))
  expect_false(any(grepl("[-']", psl$stemPro)))
  expect_equal(
    as.character(psl$pro.ipa[psl$word == "carro"][1]),
    "\u02c8ka.ro"
  )
})
