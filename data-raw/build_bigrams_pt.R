# Build bigrams_pt, the phonotactic bigram table used by biGram_pt().
#
# Bigrams are counted over the transcriptions in pt_lex (one count per
# distinct word/pronunciation pair), with word boundaries marked by ^ and $,
# and syllable boundaries and stress removed. Transcriptions are split into
# grapheme clusters, so a glide carrying a combining tilde counts as one
# segment. This matches biGram_pt_helper(), which splits its input the same way.
#
# Validation: run over the legacy pt_lex (tap written as \u027e), this reproduces
# the bigrams_pt shipped with Fonology 1.7.0 except for the bigrams affected by
# the pt_lex corrections documented in build_pt_lex.R. Ties are now ordered by
# ngram, so the row order within a tie differs from the shipped object; lookups
# are by join and do not depend on it.

pkgload::load_all(".")

segments <- stringr::str_c("^", stringr::str_remove_all(pt_lex$pro, "[.\u02c8]"), "$") |>
  stringr::str_split("")

bigrams <- unlist(lapply(segments, function(s) {
  stringr::str_c(utils::head(s, -1), utils::tail(s, -1))
}))

bigrams_pt <- tibble::tibble(ngrams = bigrams) |>
  dplyr::count(ngrams, name = "freq") |>
  dplyr::arrange(dplyr::desc(freq), ngrams) |>
  dplyr::mutate(freq = as.integer(freq), prop = freq / sum(freq))

usethis::use_data(bigrams_pt, overwrite = TRUE, compress = "bzip2")
