# Build pt_lex, the PSL-backed pronunciation lookup used by ipa(lg = "pt").
#
# pt_lex is the set of distinct (word, pronunciation) pairs in the Portuguese
# Stress Lexicon, with the PSL notation converted to IPA by psl_to_ipa(). No
# build script for it existed before Fonology 1.7.1; this one was reconstructed
# from the psl/pt_lex alignment.
#
# Validation (run before the switch to phonemic rhotics): with
# psl_to_ipa(rhotics = "legacy"), this script reproduces the pt_lex shipped with
# Fonology 1.7.0 row for row, word factor and levels included, except for 16
# rows: 12 where the shipped object wrote a nasal diphthong as \u00e3j instead of
# \u00e3j\u0303 (inconsistent with its own \u00e3w\u0303), and 4 rows for three PSL
# entries (x\u00eanico, xona, xucro) whose transcription contains the orthographic
# letter x in place of /\u0283/.

pkgload::load_all(".")

pt_lex <- tibble::tibble(word = psl$word, pro = as.character(psl$pro)) |>
  dplyr::distinct() |>
  dplyr::mutate(pro = psl_to_ipa(pro))

stopifnot(
  !anyNA(pt_lex$pro),
  !any(stringr::str_detect(pt_lex$pro, "x"))
)

usethis::use_data(pt_lex, overwrite = TRUE, compress = "bzip2")
