# Build English CMU-complement lexicon from a curated CSV.
#
# Expected source file:
# ~/Desktop/nonces_ipa.csv

source_path <- path.expand("~/Desktop/nonces_ipa.csv")

if (!file.exists(source_path)) {
  stop("Source CSV not found: ", source_path, call. = FALSE)
}

lex_en_cmu_complement <- readr::read_csv(
  source_path,
  show_col_types = FALSE
) |>
  dplyr::transmute(
    word = stringr::str_to_lower(.data$word),
    ipa = as.character(.data$ipa)
  ) |>
  dplyr::filter(
    !is.na(.data$word),
    !is.na(.data$ipa),
    stringr::str_detect(.data$word, "^[a-z']+$"),
    nzchar(.data$word),
    nzchar(.data$ipa)
  ) |>
  dplyr::distinct(.data$word, .keep_all = TRUE) |>
  dplyr::arrange(.data$word)

save(
  lex_en_cmu_complement,
  file = "data/lex_en_cmu_complement.rda",
  compress = "xz"
)
