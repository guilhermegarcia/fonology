# Build Spanish lexicon object from English-Wiktionary data (Wiktextract).
#
# Source:
# https://kaikki.org/dictionary/Spanish/kaikki.org-dictionary-Spanish.jsonl
# Wiktextract: Ylonen (2022); Wiktionary content is CC BY-SA.
#
# The download is ~966 MB; it is cached in tempdir() (or point
# KAIKKI_SPANISH_JSONL at an existing copy). Among the phonemic (/.../)
# pronunciations, the first variant WITHOUT Castilian \u03b8/\u028e is preferred,
# matching the package's seseo + ye\u00edsmo conventions; entries with only
# a Castilian variant are dropped by the character filter below.
#
# Wiktionary phonemic forms carry stress but no syllable dots. The stress
# mark always sits at a syllable boundary, so the string is split there and
# each half is syllabified with the package's own syllabify_sp() (Spanish
# syllabification is deterministic); segments and stress are Wiktionary's.

devtools::load_all()

source_url <- "https://kaikki.org/dictionary/Spanish/kaikki.org-dictionary-Spanish.jsonl"
source_path <- Sys.getenv(
  "KAIKKI_SPANISH_JSONL",
  file.path(tempdir(), "kaikki-spanish.jsonl")
)

if (!file.exists(source_path)) {
  utils::download.file(source_url, source_path, mode = "wb")
}

pairs_path <- file.path(tempdir(), "sp_word_ipa.tsv")

if (nzchar(Sys.which("jq"))) {
  system2(
    "jq",
    args = c(
      "-r",
      shQuote(paste0(
        'select(.word and .sounds) | ',
        '(.sounds | map(select(.ipa and (.ipa | startswith("/"))))) as $p | ',
        'select(($p | length) > 0) | ',
        '((($p | map(select(.ipa | test("[\u03b8\u028e]") | not))) + $p)[0].ipa) as $i | ',
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
      no_castilian <- ipas[!grepl("[\u03b8\u028e]", ipas)]
      c(word = x$word, ipa_raw = if (length(no_castilian)) no_castilian[[1]] else ipas[[1]])
    })
    parsed <- parsed[!vapply(parsed, is.null, logical(1))]
    out <- c(out, parsed)
  }
  close(con)
  raw <- tibble::as_tibble(do.call(rbind, out))
}

# Convert Wiktionary phonemic IPA to the package's Spanish conventions
wikt_to_ipa_sp <- function(x) {
  x |>
    stringr::str_remove_all("^/|/$") |>
    stringr::str_remove_all("[()]") |>
    stringr::str_remove_all("\u0361|\u035c|\u02d0|\u02d1|\u032f|\u0329|\u02c0|\u200c|\u200d|\u2060") |>
    stringr::str_replace_all("\u0261", "g") |>
    stringr::str_replace_all("\u025f\u029d", "\u029d") |>
    stringr::str_remove_all("\u02cc")
}

# Syllabify: the stress mark sits at a syllable boundary; split there and
# run each half through the package syllabifier.
syllabify_gold_sp <- function(x) {
  pre <- stringr::str_extract(x, "^[^\u02c8]*")
  post <- stringr::str_extract(x, "(?<=\u02c8)[^\u02c8]*$")

  no_stress <- is.na(post)
  out <- character(length(x))

  out[no_stress] <- syllabify_sp(x[no_stress])
  out[!no_stress] <- paste0(
    ifelse(nzchar(pre[!no_stress]), paste0(syllabify_sp(pre[!no_stress]), "."), ""),
    "\u02c8",
    syllabify_sp(post[!no_stress])
  )

  out |>
    stringr::str_replace_all("\\.{2,}", ".") |>
    stringr::str_remove("^\\.") |>
    stringr::str_remove("\\.$")
}

sp_lex <- raw |>
  dplyr::mutate(
    row_id = dplyr::row_number(),
    word = stringr::str_to_lower(.data$word),
    ipa_plain = wikt_to_ipa_sp(.data$ipa_raw)
  ) |>
  dplyr::filter(
    stringr::str_detect(.data$word, "^[a-z\u00e1\u00e9\u00ed\u00f3\u00fa\u00fc\u00f1]+$"),
    # single letters are read as letter names on Wiktionary (y = /\u029d/),
    # which clashes with their function-word homographs
    nchar(.data$word) > 1,
    stringr::str_detect(
      .data$ipa_plain,
      "^[abdefgiklmnopstuwxj\u029d\u0272\u027ersz\u0283\u02c8t]+$"
    ),
    !stringr::str_detect(.data$word, "\\d")
  ) |>
  dplyr::arrange(.data$row_id) |>
  dplyr::distinct(.data$word, .keep_all = TRUE) |>
  dplyr::mutate(ipa = syllabify_gold_sp(.data$ipa_plain)) |>
  dplyr::arrange(.data$word) |>
  dplyr::select("word", "ipa")

save(sp_lex, file = "data/sp_lex.rda", compress = "xz")
