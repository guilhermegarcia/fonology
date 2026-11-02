# Reproduces the accuracy table in README.md and the manual.
#
# Fallback accuracy: the rule-based pipeline alone, on 5 seeds of 4,000 random
# single-word lexicon entries per language, scored against the lookup form. The
# rules were written and tuned on these lexicons, so this is not an estimate on
# unseen data.
#
# Token coverage: share of running-text tokens ipa() does not star. Texts are
# Project Gutenberg plain-text files and FrequencyWords 2018 *_50k.txt lists
# (github.com/hermitdave/FrequencyWords), in two directories given on the
# command line, named as below. Not distributed with the package.
#
# Overall = coverage + (1 - coverage) * fallback. It assumes lookups are correct.
#
# Usage: Rscript data-raw/benchmark_ipa.R <gutenberg_dir> <frequencywords_dir>

pkgload::load_all(".", quiet = TRUE)
args <- commandArgs(TRUE)
ns <- asNamespace("Fonology")

fallback <- list(
  en = function(x) unname(vapply(x, ns$.fallback_to_ipa_en, "")),
  fr = function(x) ns$syllabify_fr(ns$transcribe_fr(x)),
  it = function(x) ns$stress_it(ns$syllabify_it(ns$transcribe_it(x))),
  sp = function(x) ns$stress_sp(ns$syllabify_sp(ns$transcribe_sp(x))),
  pt = function(x) {
    out <- ns$stress_pt_vec(ns$syllabify_pt_vec(ns$transcribe_pt_vec(x)))
    out <- stringr::str_remove_all(out, "\\.$")
    ns$phonemic_rhotics_pt(ns$posttonic_pt_vec(out, ortho = x))
  }
)
lexicon <- list(
  en = en_lex$word[en_lex$alt == 0], fr = fr_lex$word, it = it_lex$word,
  sp = sp_lex$word, pt = as.character(pt_lex$word)
)
layers <- list(
  en = "en_ipa_lex", fr = "fr_ipa_lex", it = c("it_ipa_lex", "it_lex_user"),
  sp = c("sp_ipa_lex", "sp_lex_user"), pt = c("pt_ipa_lex", "pt_lex_user")
)
for (lg in names(fallback)) {
  words <- unique(lexicon[[lg]])
  words <- words[stringr::str_detect(words, "^\\p{L}+$") & words == stringr::str_to_lower(words)]
  words <- setdiff(words, unlist(lapply(layers[[lg]], function(n) names(ns$.lex(n)))))
  rates <- vapply(1:5, function(seed) {
    set.seed(seed)
    sampled <- sample(words, 4000)
    gold <- stringi::stri_trans_nfc(suppressMessages(ipa(sampled, lg = lg)))
    out <- stringi::stri_trans_nfc(stringr::str_remove(suppressMessages(fallback[[lg]](sampled)), "\\*"))
    mean(out == gold, na.rm = TRUE)
  }, 0)
  cat(sprintf("fallback %s: %.1f%% (%.1f-%.1f)\n", lg, 100 * mean(rates), 100 * min(rates), 100 * max(rates)))
}

strip_gutenberg <- function(path) {
  x <- readLines(path, warn = FALSE, encoding = "UTF-8")
  paste(x[(grep("\\*\\*\\* START OF", x)[1] + 1):(grep("\\*\\*\\* END OF", x)[1] - 1)], collapse = " ")
}
coverage <- function(types, n, lg) {
  starred <- stringr::str_detect(suppressMessages(ipa(types, lg = lg)), "\\*$")
  100 * (1 - sum(n[starred], na.rm = TRUE) / sum(n))
}
texts <- list(
  en = c("1342", "84"), fr = c("17489", "13951"), sp = "2000",
  it = c("52484", "45334"), pt = c("55682", "54829")
)
for (lg in names(texts)) {
  for (id in texts[[lg]]) {
    tokens <- cleanText(
      strip_gutenberg(file.path(args[1], paste0("pg", id, ".txt"))),
      apostrophe = if (lg %in% c("fr", "it")) "keep" else "remove"
    )
    counts <- table(tokens)
    cat(sprintf("coverage %s, Gutenberg %s: %.1f%%\n", lg, id, coverage(names(counts), as.numeric(counts), lg)))
  }
}
lists <- c(en = "en", fr = "fr", sp = "es", it = "it", pt = "pt_br")
for (lg in names(lists)) {
  x <- read.table(
    file.path(args[2], paste0(lists[[lg]], "_50k.txt")),
    quote = "", comment.char = "", encoding = "UTF-8", col.names = c("w", "f"), fill = TRUE
  )
  w <- stringr::str_remove_all(stringr::str_to_lower(x$w), "[:punct:]")
  keep <- nzchar(w) & !is.na(x$f) & !stringr::str_detect(w, "\\d")
  cat(sprintf("coverage %s, subtitles: %.1f%%\n", lg, coverage(w[keep], as.numeric(x$f[keep]), lg)))
}
