# Build Italian lexicon object from English-Wiktionary data (Wiktextract).
#
# Source:
# https://kaikki.org/dictionary/Italian/kaikki.org-dictionary-Italian.jsonl
# Wiktextract: Ylonen (2022); Wiktionary content is CC BY-SA.
#
# The download is ~726 MB; it is cached in tempdir() (or point
# KAIKKI_ITALIAN_JSONL at an existing copy). Extraction uses jq when
# available (much faster) and falls back to chunked reading in R.

source_url <- "https://kaikki.org/dictionary/Italian/kaikki.org-dictionary-Italian.jsonl"
source_path <- Sys.getenv(
  "KAIKKI_ITALIAN_JSONL",
  file.path(tempdir(), "kaikki-italian.jsonl")
)

if (!file.exists(source_path)) {
  utils::download.file(source_url, source_path, mode = "wb")
}

pairs_path <- file.path(tempdir(), "it_word_ipa.tsv")

if (nzchar(Sys.which("jq"))) {
  # word + first phonemic (/.../) pronunciation, one line per entry
  system2(
    "jq",
    args = c(
      "-r",
      shQuote(paste0(
        'select(.word and .sounds) | ',
        '(.sounds | map(select(.ipa and (.ipa | startswith("/")))) | ',
        '.[0].ipa // empty) as $i | select($i != null and $i != "") | ',
        '[.word, $i] | @tsv'
      )),
      shQuote(source_path)
    ),
    stdout = pairs_path
  )
  raw <- readr::read_tsv(
    pairs_path,
    col_names = c("word", "ipa_raw"),
    show_col_types = FALSE,
    quote = ""
  )
} else {
  con <- file(source_path, open = "r")
  out <- list()
  repeat {
    lines <- readLines(con, n = 50000L, encoding = "UTF-8")
    if (length(lines) == 0L) break
    parsed <- lapply(lines, function(l) {
      x <- jsonlite::fromJSON(l, simplifyVector = FALSE)
      if (is.null(x$word) || is.null(x$sounds)) return(NULL)
      ipas <- vapply(
        x$sounds,
        function(s) if (!is.null(s$ipa)) s$ipa else "",
        character(1)
      )
      ipas <- ipas[startsWith(ipas, "/")]
      if (length(ipas) == 0L) return(NULL)
      c(word = x$word, ipa_raw = ipas[[1]])
    })
    parsed <- parsed[!vapply(parsed, is.null, logical(1))]
    out <- c(out, parsed)
  }
  close(con)
  raw <- tibble::as_tibble(do.call(rbind, out))
}

# Convert Wiktionary IPA to the package's Italian conventions
wikt_to_ipa_it <- function(x) {
  x |>
    stringr::str_remove_all("^/|/$") |>
    stringr::str_remove_all("[()\u02d0\u02d1\u0361\u035c\u032f\u0329\u02c0\u200c\u200d\u2060]") |>
    stringr::str_replace_all("\u0261", "g") |>
    # stress marks start a syllable: normalize to dot + mark
    stringr::str_replace_all("([\u02c8\u02cc])", ".\\1") |>
    stringr::str_replace_all("\\.{2,}", ".") |>
    stringr::str_remove("^\\.")
}

it_lex <- raw |>
  dplyr::mutate(
    row_id = dplyr::row_number(),
    word = stringr::str_to_lower(.data$word),
    ipa = wikt_to_ipa_it(.data$ipa_raw)
  ) |>
  dplyr::filter(
    stringr::str_detect(.data$word, "^[a-z\u00e0\u00e8\u00e9\u00ec\u00ed\u00f2\u00f3\u00f9\u00fa\u00ee]+$"),
    stringr::str_detect(
      .data$ipa,
      "^[abdefgiklmnopqrstuvwz\u025b\u0254\u0283\u0292\u0272\u028e\u014bjw.\u02c8\u02cc]+$"
    ),
    stringr::str_detect(.data$ipa, "[\u02c8]") | !stringr::str_detect(.data$ipa, "\\.")
  ) |>
  dplyr::arrange(.data$row_id) |>
  dplyr::distinct(.data$word, .keep_all = TRUE) |>
  dplyr::arrange(.data$word) |>
  dplyr::select("word", "ipa")

save(it_lex, file = "data/it_lex.rda", compress = "xz")
