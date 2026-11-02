# Regressions found while re-measuring ipa() against English Wiktionary.

test_that("Portuguese g before an accented front vowel is /ʒ/", {
  expect_equal(
    ipa(c("guerra", "guerras", "gesto", "gélido", "gíria", "analgésico"), lg = "pt"),
    c("ˈgɛ.ra", "ˈgɛ.ras", "ˈʒɛs.to",
      "ˈʒɛ.li.do", "ˈʒi.ɾja", "a.nal.ˈʒɛ.zi.ko")
  )
})

test_that("Portuguese qu/gu + en keep or drop the glide by word family", {
  expect_equal(
    stringr::str_remove(
      ipa(c("pequenos", "esquentar", "frequente", "frequência", "cinquenta", "aguentar"), lg = "pt"),
      "\\*"
    ),
    c("pe.ˈke.nos", "es.ken.ˈtaɾ", "fɾe.ˈkwen.te",
      "fɾe.ˈkwen.sja", "sin.ˈkwen.ta", "a.gwen.ˈtaɾ")
  )
  expect_equal(ipa("aquário", lg = "pt"), "a.ˈkwa.ɾjo")
})

test_that("Portuguese fallback rules", {
  words <- c("próxima", "saiu", "escolher", "irmãs", "jack")
  expected <- c("ˈpɾɔ.si.ma*", "sa.ˈiw*", "es.ko.ˈʎeɾ*", "iɾ.ˈmãs*", "ˈʒak*")
  expect_equal(ipa(words, lg = "pt"), expected)
  expect_equal(ipa("mulher", lg = "pt"), "mu.ˈʎɛɾ")
})

test_that("PSL lookups use the corrected stressed mid vowels, also in narrow", {
  expect_equal(ipa(c("papel", "melhor"), lg = "pt"), c("pa.ˈpɛl", "me.ˈʎɔɾ"))
  expect_equal(ipa("papel", lg = "pt", narrow = TRUE), "pa.ˈpɛw")
  expect_identical(as.character(psl$pro[match("papel", psl$word)]), "pa-'pel")
})

test_that("Italian function words and imperfects", {
  expect_equal(
    ipa(c("della", "nel", "cui", "col", "tue"), lg = "it"),
    c("ˈdel.la", "ˈnel", "ˈku.i", "ˈkol", "ˈtu.e")
  )
  expect_equal(ipa("sapevo", lg = "it"), "sa.ˈpe.vo*")
  expect_equal(ipa("allievo", lg = "it"), "al.ˈljɛ.vo")
})

test_that("elided clitics are transcribed with their host", {
  expect_equal(
    ipa(c("l'homme", "l’homme", "d'argent", "s'en", "jusqu'à", "aujourd'hui"), lg = "fr"),
    c("lɔm", "lɔm", "daʁ.ʒɑ̃", "sɑ̃", "ʒys.ka", "o.ʒuʁ.dɥi")
  )
  expect_equal(
    ipa(c("l'acqua", "dell'acqua", "c'era", "un'ora", "dov'è"), lg = "it"),
    c("ˈlak.kwa", "del.ˈlak.kwa", "ˈtʃɛ.ra", "u.ˈno.ra", "do.ˈvɛ")
  )
  # Unknown clitics and bare words are unaffected.
  expect_equal(ipa("sen", lg = "fr"), "sɛn")
})

test_that("cleanText() can keep apostrophes; the default still removes them", {
  expect_equal(cleanText("L'homme"), "lhomme")
  expect_equal(cleanText("L'homme d’argent, 'oui'", apostrophe = "keep"), c("l'homme", "d'argent", "oui"))
})

test_that("Spanish final diphthongs: no stray boundary, coda s stays", {
  expect_equal(
    stringr::str_remove(ipa(c("troy", "amáis", "auscultada"), lg = "sp"), "\\*"),
    c("ˈtɾoi", "a.ˈmais", "aus.kul.ˈta.da")
  )
  expect_false(any(stringr::str_detect(sp_lex$ipa, "[iu]\\.s(\\.|$)")))
})

test_that("no literal y leaks into English or Italian fallback", {
  expect_false(stringr::str_detect(ipa("hiya", lg = "en"), "y"))
  expect_false(stringr::str_detect(ipa("okay", lg = "it"), "y"))
})
