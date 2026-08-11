# Build the allFeatures object (distinctive feature table).
#
# Source: PanPhon (Mortensen et al. 2016), ipa_all.csv
# https://github.com/dmort27/panphon/blob/master/panphon/data/ipa_all.csv
#
# allFeatures is an *adaptation* of PanPhon, not a copy. The adaptations that
# predate this script are preserved by reading them back out of the existing
# data/allFeatures.rda (which is tracked in git):
#
#   * three columns are renamed: delrel -> DR, voi -> vce, velaric -> vel
#   * the velar stop is written with ASCII "g" (U+0067), not PanPhon's script
#     "ɡ" (U+0261)
#   * "back" and "tense" are hand-adapted: many consonants that PanPhon marks
#     +/- back are 0 here (clicks, taps, laterals, ɱ ʋ ɰ), and /a/ and /h/ are
#     [-back]. 3,162 "back" cells and 39 "tense" cells differ from upstream.
#   * "approx" is a 26th column with no PanPhon equivalent.
#
# This script fixes two defects in the previously shipped object:
#
#   1. 120 duplicate ipa keys (the mid-central vowels ɘ ɵ ɞ and their diacritic
#      variants, each present twice with conflicting DR/lab/back/tense values).
#      For every one of these the FIRST row matches upstream PanPhon exactly and
#      the second does not, so de-duplication keeps the first.
#   2. "strid" was 0 for all but f/v (+) and ɸ/β (-), leaving every sibilant
#      unspecified. getPhon("+strid", "pt") returned only f and v, and
#      getFeat(c("s","z","ʃ","ʒ"), "pt") returned "0strid". The column is taken
#      from upstream PanPhon, which specifies it throughout.
#   3. PanPhon has no rhoticity feature and encodes the rhotic hook by copying
#      /ɹ/'s dorsal and labial values onto the base vowel, so ə˞ and ɜ˞ (ɚ and
#      ɝ, both in the English inventory) came out [+hi] and [+round]. Height and
#      rounding are restored; [-ant] still marks them as rhotic.
#
# The ipa column is normalised to NFD so that lookups are stable regardless of
# how the caller's input is composed (.norm_ipa() in R/helper_functions.R
# applies the same normalisation to user input).

source_url <- paste0(
  "https://raw.githubusercontent.com/dmort27/panphon/master/",
  "panphon/data/ipa_all.csv"
)
source_path <- Sys.getenv(
  "PANPHON_IPA_ALL_CSV",
  file.path(tempdir(), "panphon-ipa_all.csv")
)

if (!file.exists(source_path)) {
  utils::download.file(source_url, source_path, mode = "wb")
}

panphon <- readr::read_csv(source_path, show_col_types = FALSE, progress = FALSE) |>
  dplyr::rename(DR = "delrel", vce = "voi", vel = "velaric") |>
  dplyr::mutate(
    ipa = stringr::str_replace_all(.data$ipa, "ɡ", "g"),
    ipa = stringi::stri_trans_nfd(.data$ipa)
  )

# Author-adapted columns come from the object currently in the package. Both
# sides are normalised to NFD first, which makes this script idempotent: it can
# be re-run against its own output.
load("data/allFeatures.rda")

adapted <- allFeatures |>
  dplyr::mutate(ipa = stringi::stri_trans_nfd(.data$ipa)) |>
  dplyr::distinct(.data$ipa, .keep_all = TRUE)

stopifnot(
  nrow(adapted) == nrow(panphon),
  setequal(adapted$ipa, panphon$ipa)
)

# The r-coloured vowels, adapted below; excluded from the drift check so that
# this script stays idempotent (re-running it reads back its own output).
rhotic <- stringi::stri_trans_nfd(c("ə˞", "ɜ˞"))

# Every column except strid/back/tense must already agree with upstream; if a
# future PanPhon release changes one of them, fail loudly rather than silently
# rewriting the table.
shared <- setdiff(names(panphon), c("ipa", "strid", "back", "tense"))
check <- dplyr::inner_join(
  adapted[c("ipa", shared)], panphon[c("ipa", shared)],
  by = "ipa", suffix = c(".pkg", ".up")
) |>
  dplyr::filter(!.data$ipa %in% rhotic)
drifted <- shared[vapply(
  shared,
  function(f) any(check[[paste0(f, ".pkg")]] != check[[paste0(f, ".up")]]),
  logical(1)
)]
if (length(drifted) > 0) {
  stop(
    "Upstream PanPhon has changed columns that were assumed stable: ",
    paste(drifted, collapse = ", ")
  )
}

allFeatures <- adapted |>
  dplyr::select(-"strid") |>
  dplyr::left_join(panphon[c("ipa", "strid")], by = "ipa") |>
  dplyr::select(dplyr::all_of(names(adapted))) |>
  tibble::as_tibble()

# The r-coloured vowels ə˞ and ɜ˞ (written ɚ and ɝ in the English inventory).
# PanPhon has no rhoticity feature, so it marks the rhotic hook by copying /ɹ/'s
# dorsal and labial values onto the base vowel, which makes ɚ and ɝ come out
# [+hi] and [+round]. That is wrong on its face - both are mid and unrounded -
# and it put them in the high-vowel class of every English computation. Their
# height and rounding are restored here; [-ant], also from the hook, is left in
# place and is what keeps them distinct from plain ə and ɜ.
allFeatures <- allFeatures |>
  dplyr::mutate(
    hi = ifelse(.data$ipa %in% rhotic, "-", .data$hi),
    round = ifelse(.data$ipa %in% rhotic, "-", .data$round)
  )

stopifnot(
  all(allFeatures$ant[allFeatures$ipa %in% rhotic] == "-"),
  sum(allFeatures$ipa %in% rhotic) == 2
)

stopifnot(
  !anyDuplicated(allFeatures$ipa),
  nrow(allFeatures) == 6367,
  ncol(allFeatures) == 26,
  identical(names(allFeatures), names(adapted)),
  !anyNA(allFeatures),
  all(unlist(allFeatures[-1]) %in% c("+", "-", "0")),
  identical(allFeatures$ipa, stringi::stri_trans_nfd(allFeatures$ipa))
)

save(allFeatures, file = "data/allFeatures.rda", compress = "xz")
