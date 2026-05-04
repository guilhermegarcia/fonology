# Build French lexicon object from Lexique 4.
#
# Source:
# http://www.lexique.org/databases/Lexique400/Lexique400.zip

source_url <- "http://www.lexique.org/databases/Lexique400/Lexique400.zip"
source_zip <- file.path(tempdir(), "Lexique400.zip")
source_path <- file.path(tempdir(), "Lexique4.tsv")

if (!file.exists(source_zip)) {
  utils::download.file(source_url, source_zip, mode = "wb")
}

if (!file.exists(source_path)) {
  utils::unzip(source_zip, files = "Lexique4.tsv", exdir = tempdir())
}

lexique_to_ipa_fr <- function(x) {
  replacements <- c(
    "S" = "\u0283",
    "Z" = "\u0292",
    "R" = "\u0281",
    "N" = "\u0272",
    "G" = "\u014b",
    "E" = "\u025b",
    "O" = "\u0254",
    "2" = "\u00f8",
    "9" = "\u0153",
    "5" = "\u025b\u0303",
    "1" = "\u0153\u0303",
    "@" = "\u0251\u0303",
    "\u00a7" = "\u0254\u0303",
    "8" = "\u0265",
    "\u00b0" = "\u0259",
    "-" = "."
  )

  out <- x
  for (pattern in names(replacements)) {
    out <- stringr::str_replace_all(
      out,
      stringr::fixed(pattern),
      replacements[[pattern]]
    )
  }

  out
}

lexique <- readr::read_tsv(
  source_path,
  show_col_types = FALSE,
  locale = readr::locale(encoding = "UTF-8")
)

fr_lex <- lexique |>
  dplyr::mutate(
    row_id = dplyr::row_number(),
    word = stringr::str_to_lower(.data[["1_Mot"]]),
    ipa = lexique_to_ipa_fr(.data[["25_SyllPhono"]]),
    phon_lexique = .data[["2_Phono"]],
    ipa_plain = .data[["3_Phono_IPA"]],
    syll_lexique = .data[["25_SyllPhono"]],
    islem = as.integer(.data[["14_IsLem"]]),
    freq_mot = as.numeric(.data[["10_FreqMot"]]),
    freq_ortho = as.numeric(.data[["11_FreqOrtho"]]),
    cd_ortho = as.numeric(.data[["13_CDOrtho"]])
  ) |>
  dplyr::filter(
    !is.na(.data$word),
    !is.na(.data$ipa),
    nzchar(.data$word),
    nzchar(.data$ipa),
    !stringr::str_detect(.data$word, "\\d")
  ) |>
  dplyr::distinct(.data$word, .data$ipa, .keep_all = TRUE) |>
  dplyr::arrange(
    .data$word,
    dplyr::desc(.data$freq_mot),
    dplyr::desc(.data$freq_ortho),
    dplyr::desc(.data$cd_ortho),
    dplyr::desc(.data$islem),
    .data$row_id
  ) |>
  dplyr::distinct(.data$word, .keep_all = TRUE) |>
  dplyr::arrange(.data$word) |>
  dplyr::select(
    "word",
    "ipa",
    "phon_lexique",
    "ipa_plain",
    "syll_lexique"
  )

save(fr_lex, file = "data/fr_lex.rda", compress = "xz")
