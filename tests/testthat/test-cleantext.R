# Regression tests for cleanText() and strip_clitic_pt().
#
# Background, all verified on the SUBTLEX-PT word list:
#
#  1. cleanText() replaced hyphens with spaces on its first line and called
#     strip_clitic_pt() near the end, so clitic removal could never fire.
#  2. strip_clitic_pt() also carried a list of hyphenated PREFIXES. Making the
#     call reachable would have deleted real derivational material from 962 of
#     the 2,964 hyphenated forms in SUBTLEX-PT ("anti-aborto" -> "aborto"),
#     while affecting zero enclitics, because that corpus lists none. The
#     prefix list was removed rather than switched on.
#  3. cleanText() had separate code paths for length-1 and length-n input with
#     different policies for degenerate tokens: dropped at length 1, NA
#     otherwise.

test_that("the default is unchanged: hyphens split, clitics untouched", {
  expect_equal(cleanText("guarda-chuva"), c("guarda", "chuva"))
  expect_equal(cleanText("MOBY-DICK"), c("moby", "dick"))
  expect_equal(cleanText("anti-aborto"), c("anti", "aborto"))
  expect_equal(cleanText("diz-me"), c("diz", "me"))
  expect_equal(cleanText("v\u00ea-los"), c("v\u00ea", "los"))
})

test_that("derivational prefixes are never deleted", {
  # The regression this guards against: "anti-aborto" must not become "aborto".
  for (w in c("anti-aborto", "ex-marido", "pr\u00e9-escolar", "al\u00e9m-mar", "sub-regi\u00e3o")) {
    out <- cleanText(w)
    expect_length(out, 2)
  }
})

test_that("hyphen = 'join' returns one token", {
  expect_equal(cleanText("guarda-chuva", hyphen = "join"), "guardachuva")
  expect_equal(cleanText("anti-aborto",  hyphen = "join"), "antiaborto")
})

test_that("hyphen = 'keep' preserves the hyphen through punctuation stripping", {
  expect_equal(cleanText("guarda-chuva", hyphen = "keep"), "guarda-chuva")
  expect_equal(cleanText("guarda-chuva!", hyphen = "keep"), "guarda-chuva")
})

test_that("hyphen must be one of the documented values", {
  expect_error(cleanText("casa", hyphen = "explode"))
})

test_that("clitics = TRUE removes the enclitic, longest match first", {
  # The alternation used to list -lo before -los, so -los matched as -lo and
  # left a stranded consonant: "v\u00ea-los" came back as "VES".
  expect_equal(cleanText("v\u00ea-los", clitics = TRUE), "v\u00ea")
  expect_equal(cleanText("v\u00ea-las", clitics = TRUE), "v\u00ea")
  expect_equal(cleanText("am\u00e1-los", clitics = TRUE), "am\u00e1")
  expect_equal(cleanText("diz-me",  clitics = TRUE), "diz")
})

test_that("clitics = TRUE works inside a running sentence", {
  # The pattern is anchored, so removal has to happen per token.
  expect_equal(
    cleanText("Ele disse-me que vem.", clitics = TRUE),
    c("ele", "disse", "que", "vem")
  )
})

test_that("clitics = TRUE still leaves non-clitic compounds alone", {
  expect_equal(cleanText("guarda-chuva", clitics = TRUE), c("guarda", "chuva"))
  expect_equal(cleanText("anti-aborto",  clitics = TRUE), c("anti", "aborto"))
})

test_that("strip_clitic_pt is anchored and vectorised", {
  s <- Fonology:::strip_clitic_pt
  expect_equal(s("ontem"), "ontem")       # ends in -em, not -me
  expect_equal(s("paladar"), "paladar")
  expect_equal(s(c("diz-me", "casa", "v\u00ea-los")), c("diz", "casa", "v\u00ea"))
})

test_that("the enclitic paradigm is complete", {
  # Measured on ~18h of transcribed speech per variety, the old list matched
  # only 40% of enclitic occurrences: -se (2,762) and -nos (1,401) were the
  # two largest gaps, and -se is the most frequent clitic in the language.
  s <- Fonology:::strip_clitic_pt
  expect_equal(s("refere-se"), "refere")        # reflexive
  expect_equal(s("deu-nos"), "deu")             # dative
  expect_equal(s("dar-vos"), "dar")
  expect_equal(s("retirar-lhes"), "retirar")
  expect_equal(s("p\u00f5e-no"), "p\u00f5e")               # allomorph after a nasal
  expect_equal(s("d\u00e1-mo"), "d\u00e1")                 # contracted dative+accusative
  expect_equal(s("d\u00e1-lho"), "d\u00e1")
})

test_that("a sequence of enclitics is removed in one pass", {
  s <- Fonology:::strip_clitic_pt
  expect_equal(s("parece-se-me"), "parece")
  expect_equal(s("viver-se-me"), "viver")
  expect_equal(s("dar-te-lhe"), "dar")
})

test_that("a token made only of clitics is returned unchanged", {
  # Morphologically blind code should degrade by leaving the input alone,
  # not by deleting it.
  s <- Fonology:::strip_clitic_pt
  expect_equal(s("se-se"), "se")
  expect_equal(s("me-o"), "me")
  expect_false(any(s(c("se-se", "a-o")) == ""))
})

test_that("clitic removal does not touch hyphenated compounds", {
  # The package has no morphological analyser, so the only guard against
  # eating a compound is that its final element is not spelled like a clitic.
  # These are the cases that matter in practice.
  s <- Fonology:::strip_clitic_pt
  compounds <- c("guarda-chuva", "bem-te-vi", "p\u00e9-de-moleque", "arco-\u00edris",
                 "couve-flor", "porta-voz", "ex-marido", "anti-aborto",
                 "sof\u00e1-cama", "luso-brasileiro", "m\u00e3o-de-obra", "vice-rei",
                 "abelha-mestra")
  expect_equal(s(compounds), compounds)
})

test_that("strip_clitic_pt no longer knows about prefixes", {
  s <- Fonology:::strip_clitic_pt
  expect_equal(s("anti-aborto"), "anti-aborto")
  expect_equal(s("ex-aluno"), "ex-aluno")
  expect_equal(s("al\u00e9m-mar"), "al\u00e9m-mar")
})

test_that("-ao is not in the list: it is a verb ending, not a clitic", {
  # It was in the old list, matched zero occurrences in either corpus, and
  # invited confusion with the mesoclitic future (far-lhe-ao).
  s <- Fonology:::strip_clitic_pt
  expect_equal(s("gr\u00e3o-\u00e3o"), "gr\u00e3o-\u00e3o")
})

test_that("degenerate tokens are treated the same at any input length", {
  # Previously: character(0) at length 1, but NA inside a longer vector.
  expect_equal(cleanText("123"), character(0))
  expect_equal(cleanText(c("123", "casa")), "casa")
  expect_equal(cleanText("..."), character(0))
  expect_equal(cleanText(c("...", "casa")), "casa")
})

test_that("the result never contains NA or empty strings", {
  out <- cleanText(c("a", "...", "b", NA, "c", "", "123"))
  expect_false(any(is.na(out)))
  expect_false(any(out == ""))
  expect_equal(out, c("a", "b", "c"))
})

test_that("documented behaviour on mixed text is unchanged", {
  expect_equal(
    cleanText("Este \u00e9 um texto em portugu\u00eas? This is a text in English!"),
    c("este", "\u00e9", "um", "texto", "em", "portugu\u00eas",
      "this", "is", "a", "text", "in", "english")
  )
})

test_that("the lookup/fallback star marker survives", {
  expect_equal(cleanText("*casa"), "*casa")
})

test_that("stress marks and syllable punctuation are removed", {
  expect_equal(cleanText("ka.\u02c8fa"), "kafa")
})

test_that("nGramTbl never receives a literal NA token", {
  # Consequence of the old length-n NA policy: nGramTbl wraps each token as
  # "^token$", so an NA became the three-character string "^NA$".
  out <- nGramTbl(c("kasa", "...", "bola"), n = 2)
  expect_false(any(grepl("N", out[[1]], fixed = TRUE)))
})
