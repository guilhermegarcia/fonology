# Build English lexicon objects from the seminar-derived CMU table.
#
# Expected source file:
# /Users/gdgarcia/Library/CloudStorage/Dropbox/Academia/Research/Events/ERASMUS+/seminar/English-CMU/cmu_en_ipa_syll.RData

source_path <- "/Users/gdgarcia/Library/CloudStorage/Dropbox/Academia/Research/Events/ERASMUS+/seminar/English-CMU/cmu_en_ipa_syll.RData"

if (!file.exists(source_path)) {
  stop("Source RData not found: ", source_path, call. = FALSE)
}

load(source_path)

en_lex <- cmu_en_ipa_syll |>
  dplyr::mutate(
    word = stringr::str_to_lower(.data$word),
    ipa_syll = unname(.data$ipa_syll)
  ) |>
  dplyr::filter(stringr::str_detect(.data$word, "^[a-z']+$")) |>
  dplyr::arrange(.data$word, .data$alt)

en_ipa_lex <- stats::setNames(character(0), character(0))

save(en_lex, file = "data/en_lex.rda", compress = "xz")
save(en_ipa_lex, file = "data/en_ipa_lex.rda", compress = "xz")
