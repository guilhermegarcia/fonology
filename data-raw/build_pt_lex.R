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

# Corrections to the PSL's transcriptions (Fonology 1.9.0). The PSL is kept
# untouched in `psl`; these apply to the lookup only.
#
# 1. An accented g + front vowel is /\u0292/ in the spelling's only reading
#    (analg\u00e9sico, evang\u00e9lico, apolog\u00e9tico), but the PSL has /g/. The accent marks
#    primary stress, so only the stressed onset changes.
accented_g <- stringr::str_detect(as.character(pt_lex$word), "g[\u00e9\u00ea\u00ed]")
pt_lex$pro[accented_g] <- stringr::str_replace(
  pt_lex$pro[accented_g], "\u02c8g(?=[e\u025bi])", "\u02c8\u0292"
)

# 2. Stressed mid vowels that differ from an unambiguous general-Brazil
#    Wiktionary pronunciation (papel, melhor, futebol).
mid_fixes <- readr::read_tsv(
  "data-raw/pt_psl_corrections.tsv",
  show_col_types = FALSE, quote = ""
)
i <- match(mid_fixes$word, as.character(pt_lex$word))
stopifnot(!anyNA(i), identical(pt_lex$pro[i], mid_fixes$from))
pt_lex$pro[i] <- mid_fixes$to

stopifnot(
  !anyNA(pt_lex$pro),
  !any(stringr::str_detect(pt_lex$pro, "x"))
)

usethis::use_data(pt_lex, overwrite = TRUE, compress = "bzip2")
