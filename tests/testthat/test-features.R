# Regression tests for getFeat() and getPhon().
#
# Most of these encode a specific defect found in the 2026-08 audit; the
# property tests at the bottom are what would have caught them in the first
# place.

# ---- the velar stop is in every inventory ----------------------------------
# allFeatures writes it "g" (U+0067) while getFeat's old segment allow-list
# wrote it "\u0261" (script g), so /g/ was filtered out of every language and
# getFeat silently returned the matrix for the remaining phonemes.

test_that("getFeat sees the velar stop", {
  expect_false(
    identical(getFeat("g", "italian"), "Not a natural class in this language.")
  )
  expect_false(identical(getFeat("k", "italian"), getFeat(c("k", "g"), "italian")))
  expect_equal(getFeat(c("k", "g"), "italian"), c("+cons", "+back"))
})

test_that("getFeat and getPhon agree about the velar stop", {
  expect_setequal(getPhon(getFeat(c("k", "g"), "italian"), "italian"), c("k", "g"))
  expect_setequal(getPhon(c("+cons", "+back"), "italian"), c("k", "g"))
})

test_that("getFeat does not invent features for phonemes it dropped", {
  # "-cor" used to appear here only because /g/ had been removed from the table.
  expect_equal(getFeat("b", "portuguese"), c("-son", "-cont", "+vce", "+lab"))
  expect_equal(getFeat(c("b", "d", "g"), "portuguese"), c("-son", "-cont", "+vce"))
})

# ---- feature names are matched exactly -------------------------------------
# The old regex alternation was leftmost-first, so "long" matched "lo" and
# "hitone"/"hireg" matched "hi".

test_that("getPhon does not confuse long/lo and hitone/hireg/hi", {
  expect_false(identical(getPhon("+long", "english"), getPhon("+lo", "english")))
  expect_false(identical(getPhon("+hitone", "english"), getPhon("+hi", "english")))
  expect_false(identical(getPhon("+hireg", "english"), getPhon("+hi", "english")))
})

test_that("every documented feature name is accepted", {
  for (feature in .feature_names) {
    expect_no_error(getPhon(stringr::str_c("+", feature), "portuguese"))
    expect_no_error(getPhon(stringr::str_c("0", feature), "portuguese"))
  }
})

test_that("getPhon rejects malformed and repeated features", {
  expect_error(getPhon("hi", "english"), "Incorrect feature")
  expect_error(getPhon("*hi", "english"), "Incorrect feature")
  expect_error(getPhon("+nope", "english"), "Incorrect feature")
  expect_error(getPhon(c("+hi", "+hi"), "english"), "only be given once")
  expect_error(getPhon(c("+hi", "-hi"), "english"), "only be given once")
})

# ---- French nasal vowels ---------------------------------------------------
# \u0251\u0303 and \u0254\u0303 were missing from the old allow-list, so getFeat
# denied they existed while getPhon returned them.

test_that("French nasal vowels are available to both functions", {
  expect_false(identical(
    getFeat("\u0254\u0303", "french"), "Not a natural class in this language."
  ))
  expect_equal(getFeat(c("\u025b\u0303", "\u0153\u0303", "\u0254\u0303", "\u0251\u0303"), "french"), c("+syl", "+nas"))
  expect_setequal(
    getPhon(c("+syl", "+nas"), "french"),
    c("\u025b\u0303", "\u0153\u0303", "\u0254\u0303", "\u0251\u0303")
  )
})

test_that("the French nasals form a natural class", {
  expect_equal(getFeat(c("m", "n", "\u0272", "\u014b"), "french"), c("-syl", "+nas"))
})

# ---- inventories -----------------------------------------------------------

test_that("both functions draw on the same inventory", {
  for (lg in c("pt", "sp", "fr", "it", "en")) {
    inventory <- .inventory(lg)
    expect_setequal(getPhon("+syl", lg), inventory[inventory %in% getPhon("+syl", lg)])
    # every segment of the inventory is reachable
    expect_setequal(
      c(getPhon("+syl", lg), getPhon("-syl", lg)),
      inventory
    )
  }
})

test_that("every inventory segment exists in allFeatures", {
  for (lg in c("pt", "sp", "fr", "it", "en")) {
    expect_no_error(.feature_table(.inventory(lg)))
  }
})

test_that("Spanish has /n/", {
  expect_true("n" %in% .inventory("sp"))
})

test_that("allFeatures has no duplicate symbols", {
  expect_equal(sum(duplicated(allFeatures$ipa)), 0)
  expect_true(all(unlist(allFeatures[-1]) %in% c("+", "-", "0")))
})

# ---- Unicode normalisation and alias spellings -----------------------------

test_that("precomposed and decomposed input give the same answer", {
  precomposed <- "\u00e3" # a-tilde as one code point
  decomposed <- "a\u0303" # a + combining tilde

  expect_false(identical(precomposed, decomposed))
  expect_equal(.norm_ipa(precomposed), .norm_ipa(decomposed))

  # Either spelling works, in the input and in the inventory.
  expect_equal(
    getFeat(precomposed, c(precomposed, "a", "p")),
    getFeat(decomposed, c(decomposed, "a", "p"))
  )
  expect_equal(getFeat(precomposed, c(decomposed, "a", "p")), c("+nas"))
})

test_that("affricates work with and without a tie bar", {
  expect_equal(getFeat("t\u0283", "english"), getFeat("t\u0361\u0283", "english"))
  expect_equal(getFeat("d\u0292", "english"), getFeat("d\u0361\u0292", "english"))
})

test_that("segments emitted by ipa() are usable", {
  # Portuguese /x/, Spanish /n/, English r-coloured vowels.
  expect_no_error(getFeat("x", "pt"))
  expect_no_error(getFeat("n", "sp"))
  expect_equal(getFeat(c("\u025a", "\u025d"), "en"), c("+syl", "-ant"))
})

test_that("Portuguese vowels are all oral", {
  # Nasal vowels are treated as derived from vowel + nasal, so they are not in
  # the inventory; a nasal vowel is rejected with its own symbol in the message.
  expect_false(any(stringi::stri_trans_nfd("\u00e3") == .norm_ipa(.inventory("pt"))))
  expect_error(getFeat("\u00e3", "pt"), "doesn't match phonemic inventory")
  expect_equal(
    getPhon(c("+syl", "+nas"), "pt"),
    "No phonemes with the features provided given the inventory in question."
  )
  # With no nasal vowels around, [+nas] alone picks out the nasal consonants.
  expect_equal(getFeat(c("m", "n", "\u0272"), "pt"), "+nas")
})

test_that("the r-coloured vowels are not high or rounded", {
  expect_false("\u025a" %in% getPhon("+hi", "en"))
  expect_false("\u025a" %in% getPhon("+round", "en"))
  expect_equal(getPhon(c("+hi", "+tense"), "english"), c("i", "u"))
})

# ---- strid is a usable feature ---------------------------------------------

test_that("the sibilants are specified for strid", {
  expect_setequal(
    getPhon("+strid", "pt"),
    c("f", "v", "s", "z", "\u0283", "\u0292")
  )
  expect_equal(getFeat(c("s", "z", "\u0283", "\u0292"), "pt"), c("+strid", "+cor"))
})

# ---- degenerate input does not crash ---------------------------------------

test_that("the whole inventory is handled", {
  expect_equal(
    getFeat(.inventory("pt"), "pt"),
    "No distinguishing features: this is the entire inventory."
  )
})

test_that("a one-segment inventory is handled", {
  expect_equal(getFeat("i", c("i", "a")), "+hi")
  expect_error(getFeat("i", c("i", "NOTIPA")), "absent from allFeatures")
  expect_error(getPhon("+syl", c("i", "NOTIPA")), "absent from allFeatures")
})

test_that("empty input errors clearly", {
  expect_error(getFeat(character(0), "english"), "No phonemes provided")
  expect_error(getPhon(character(0), "english"), "No features provided")
})

test_that("unknown segments are reported, never dropped", {
  expect_error(getFeat("q", "portuguese"), "doesn't match phonemic inventory")
  expect_error(getFeat("p\u02b0", "portuguese"), "doesn't match phonemic inventory")
})

test_that("an unsupported language errors", {
  expect_error(getFeat("i", "klingon"), "Language not supported")
  expect_error(getPhon("+syl", "klingon"), "Language not supported")
})

# ---- return-value contract -------------------------------------------------

test_that("the documented sentinel strings are unchanged", {
  expect_equal(
    getFeat(c("i", "u"), "french"), "Not a natural class in this language."
  )
  expect_equal(
    getPhon("+cg", "english"),
    "No phonemes with the features provided given the inventory in question."
  )
})

test_that("the documented examples still hold", {
  expect_equal(getFeat(c("i", "u"), "english"), c("+hi", "+tense"))
  expect_equal(getFeat(c("i", "y", "u"), "french"), c("+syl", "+hi"))
  expect_equal(getFeat(c("p", "b"), "portuguese"), c("-son", "-cont", "+lab"))
  expect_setequal(getPhon(c("+syl", "+hi"), "french"), c("i", "y", "u"))
  expect_setequal(
    getPhon(c("-son", "+vce"), "spanish"),
    c("b", "v", "d", "g", "z", "\u029d")
  )
})

test_that("getPhon returns phonemes in inventory order", {
  expect_equal(getPhon("+syl", "it"), .inventory("it")[seq_len(7)])
})

# ---- minimal matrices are minimal and informative --------------------------

test_that("a specified feature is preferred over a vacuous one", {
  # 0DR also isolates the French low nasal vowel, and DR comes first in feature
  # order, but it says nothing about the segment.
  expect_equal(getFeat("\u0251\u0303", "french"), c("+nas", "+lo"))
})

test_that("classic natural classes come out right", {
  expect_equal(getFeat(c("p", "t", "k"), "pt"), c("-cont", "-vce"))
  expect_equal(getFeat(c("m", "n", "\u0272"), "pt"), "+nas")
  expect_equal(getFeat(c("l", "r", "\u027e", "\u028e"), "pt"), c("+cons", "+approx"))
  expect_equal(getFeat(c("f", "v", "s", "z", "\u0283", "\u0292", "x"), "pt"), c("-son", "+cont"))
  expect_equal(
    getFeat(c("t\u0361\u0283", "d\u0361\u0292", "t\u0361s", "d\u0361z"), "it"), "+DR"
  )
})

# ---- property tests --------------------------------------------------------
# These are the general statements the regression tests above are instances of.

test_that("getFeat and getPhon are inverses on every one- and two-phoneme set", {
  sentinels <- c(
    "Not a natural class in this language.",
    "No distinguishing features: this is the entire inventory."
  )

  for (lg in c("pt", "sp", "fr", "it", "en")) {
    inventory <- .inventory(lg)
    sets <- c(
      lapply(seq_along(inventory), function(i) i),
      utils::combn(length(inventory), 2, simplify = FALSE)
    )

    for (indices in sets) {
      phonemes <- inventory[indices]
      features <- getFeat(phonemes, lg)

      if (length(features) == 1 && features %in% sentinels) {
        next
      }

      expect_setequal(getPhon(features, lg), phonemes)
    }
  }
})

test_that("a feature matrix is never larger than it needs to be", {
  for (lg in c("pt", "it")) {
    inventory <- .inventory(lg)

    for (indices in utils::combn(length(inventory), 2, simplify = FALSE)) {
      phonemes <- inventory[indices]
      features <- getFeat(phonemes, lg)

      if (length(features) < 2) {
        next
      }

      # No proper subset of the matrix may pick out the same phonemes.
      for (drop in seq_along(features)) {
        expect_false(setequal(getPhon(features[-drop], lg), phonemes))
      }
    }
  }
})
