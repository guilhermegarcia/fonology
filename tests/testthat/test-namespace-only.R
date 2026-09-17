test_that("exported data-backed functions work without attaching Fonology", {
  caches <- list(.sp_cache, .fr_cache, .it_cache, .en_cache, .fonology_state)
  for (cache in caches) {
    rm(list = ls(envir = cache, all.names = TRUE), envir = cache)
  }

  was_attached <- "package:Fonology" %in% search()
  if (was_attached) {
    detach("package:Fonology", unload = FALSE, character.only = TRUE)
    # Re-attach the namespace that is already loaded. library() would attach
    # whichever version is installed, which under devtools::test() is not
    # necessarily the one being tested, and every later test file would then
    # run against it.
    on.exit(
      suppressPackageStartupMessages(attachNamespace("Fonology")),
      add = TRUE
    )
  }

  expect_equal(Fonology::ipa("casa", lg = "pt"), "\u02c8ka.za")
  expect_equal(Fonology::ipa("casa", lg = "sp"), "\u02c8ka.sa")
  expect_equal(Fonology::ipa("femme", lg = "fr"), "fam")
  expect_equal(Fonology::ipa("casa", lg = "it"), "\u02c8ka.sa")
  expect_equal(Fonology::ipa("hospital", lg = "en"), "\u02c8h\u0251s.p\u026a.t\u0259l")

  expect_no_error(Fonology::getFeat("i", lg = "sp"))
  expect_no_error(Fonology::getPhon("+syl", lg = "sp"))
  expect_equal(Fonology::getWeight("i.ta.li.\u02c8a.no", lg = "it"), "LLL")
  expect_type(Fonology::biGram_pt("paklode"), "double")
})
