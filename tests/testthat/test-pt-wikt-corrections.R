test_that("audited Portuguese corrections fix high-impact broad forms", {
  words <- c(
    "agora", "quer", "quero", "posso", "porque", "sair", "podem",
    "querem", "fazem", "homens"
  )
  expected <- c(
    "a.ˈgɔ.ra", "ˈkɛr", "ˈkɛ.ro", "ˈpɔ.so", "por.ˈke", "sa.ˈir",
    "ˈpɔ.dem", "ˈkɛ.rem", "ˈfa.zem", "ˈo.mens"
  )

  expect_equal(ipa(words, lg = "pt"), expected)
  expect_true(all(Fonology:::.is_pt_broad_ipa(expected)))
})

test_that("Portuguese correction data are valid and broad exhaustively", {
  fix <- Fonology:::.get_pkg_data("pt_lex_fix")

  expect_length(fix, 264)
  expect_identical(anyDuplicated(names(fix)), 0L)
  expect_true(all(stringi::stri_trans_nfc(names(fix)) == names(fix)))
  expect_true(all(stringr::str_detect(
    names(fix),
    "^[a-záéíóúâêôãõçü]+$"
  )))
  expect_true(all(stringi::stri_trans_nfc(unname(fix)) == unname(fix)))

  output <- ipa(names(fix), lg = "pt")
  expect_false(any(stringr::str_detect(output, stringr::fixed("*"))))
  expect_true(all(Fonology:::.is_pt_broad_ipa(output)))

  ipa_fix <- Fonology:::.get_pkg_data("pt_ipa_fix")
  expect_true(all(Fonology:::.is_pt_broad_ipa(unname(ipa_fix))))
})

test_that("surface allophones remain confined to narrow Portuguese output", {
  broad <- ipa(c("tivesse", "quero"), lg = "pt", narrow = FALSE)
  narrow <- ipa(c("tivesse", "quero"), lg = "pt", narrow = TRUE)

  expect_equal(broad, c("ti.ˈvɛ.se", "ˈkɛ.ro"))
  expect_true(all(Fonology:::.is_pt_broad_ipa(broad)))
  expect_false(any(stringr::str_detect(broad, "[ɪʊ͡]")))
  expect_true(any(stringr::str_detect(narrow, "[ɪʊ͡]")))
})

test_that("local Portuguese entries still override audited corrections", {
  old <- Fonology:::.get_user_lex("pt_lex_user")
  on.exit(Fonology:::.set_user_lex("pt_lex_user", old), add = TRUE)

  local <- c(old[names(old) != "agora"], agora = "agôra")
  Fonology:::.set_user_lex("pt_lex_user", local)

  expect_equal(ipa("agora", lg = "pt"), "a.ˈgo.ra")
})

test_that("PSL precedence and starred fallback behaviour are unchanged", {
  expect_equal(
    ipa(c("casa", "abacaxi", "agosto"), lg = "pt"),
    c("ˈka.za", "a.ba.ka.ˈʃi", "a.ˈgos.to")
  )
  expect_true(stringr::str_ends(
    ipa("facebook", lg = "pt"), stringr::fixed("*")
  ))
  expect_true(stringr::str_ends(
    ipa("flopranto", lg = "pt"), stringr::fixed("*")
  ))
})

test_that("Portuguese shipped lookup covers at least 28 percent of tokens", {
  pt_lex <- Fonology:::.get_pkg_data("pt_lex")
  pt_fix <- Fonology:::.get_pkg_data("pt_lex_fix")
  freq <- Fonology:::.get_pkg_data("pt_freq")
  lookup <- union(stringr::str_to_lower(pt_lex$word), names(pt_fix))

  coverage <- sum(freq$freq[stringr::str_to_lower(freq$word) %in% lookup]) /
    sum(freq$freq)

  expect_gte(coverage, 0.28)
  expect_lt(coverage, 0.29)
})

test_that("component-level data licensing is installed", {
  note <- file.path(Fonology:::.pkg_dir(), "LICENSE.note")
  expect_true(file.exists(note))
  text <- paste(readLines(note, warn = FALSE), collapse = "\n")
  expect_match(text, "pt_lex_fix\\.rda")
  expect_match(text, "CC BY-SA 4\\.0")
  expect_match(text, "Wiktionary contributors")
})
