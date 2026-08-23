# Build a small, accuracy-bearing Portuguese correction complement from
# English Wiktionary data extracted by Wiktextract/Kaikki.
#
# This deliberately does NOT ship a Portuguese Wiktionary pronunciation
# lexicon.  Wiktionary is used to find cases where the PSL + regex pipeline is
# wrong in an independently observable broad property (primary-stress
# placement or stressed oral-mid-vowel quality).  Only enough audited,
# reproducible corrections to cover 95% of that correction-bearing token mass
# are added to the existing shipped correction layers.
#
# Source (CC BY-SA 4.0):
# https://kaikki.org/dictionary/Portuguese/kaikki.org-dictionary-Portuguese.jsonl
#
# Set KAIKKI_PORTUGUESE_TSV to a pre-extracted four-column TSV with columns
# word, POS, comma-separated sound tags, and IPA.  Otherwise set
# KAIKKI_PORTUGUESE_JSONL to a cached JSONL file; if neither is set, the JSONL
# is downloaded to tempdir().

devtools::load_all()

source_url <- paste0(
  "https://kaikki.org/dictionary/Portuguese/",
  "kaikki.org-dictionary-Portuguese.jsonl"
)
source_path <- Sys.getenv(
  "KAIKKI_PORTUGUESE_JSONL",
  file.path(tempdir(), "kaikki-portuguese.jsonl")
)
pairs_path <- Sys.getenv(
  "KAIKKI_PORTUGUESE_TSV",
  file.path(tempdir(), "pt_word_pos_tags_ipa.tsv")
)

if (!nzchar(Sys.getenv("KAIKKI_PORTUGUESE_TSV"))) {
  if (!file.exists(source_path)) {
    utils::download.file(source_url, source_path, mode = "wb")
  }
  if (!nzchar(Sys.which("jq"))) {
    stop("Building Portuguese corrections requires jq or KAIKKI_PORTUGUESE_TSV.")
  }

  expr <- paste0(
    'select(.word and .sounds) as $e | .sounds[]? | ',
    'select(.ipa and (.ipa | startswith("/"))) | ',
    '[$e.word, $e.pos, (.tags // [] | join(",")), .ipa] | @tsv'
  )
  status <- system2(
    "jq",
    args = c("-r", shQuote(expr), shQuote(source_path)),
    stdout = pairs_path
  )
  if (!identical(status, 0L)) stop("jq extraction failed with status ", status)
}

raw <- readr::read_tsv(
  pairs_path,
  col_names = c("word", "pos", "tags", "ipa_source"),
  show_col_types = FALSE,
  quote = ""
) |>
  dplyr::mutate(
    word = stringi::stri_trans_nfc(stringr::str_to_lower(.data$word)),
    ipa_source = stringi::stri_trans_nfc(.data$ipa_source)
  ) |>
  dplyr::filter(
    .data$tags == "Brazil",
    stringr::str_detect(
      .data$word,
      "^[a-záéíóúâêôãõçü]+$"
    )
  )

stress_index <- function(x) {
  x <- x |>
    stringr::str_remove_all("^/|/$") |>
    # Both stress marks begin a syllable in Wiktionary's unsyllabified
    # notation. Secondary stress must establish its boundary before it is
    # ignored; otherwise compounds and -mente adverbs appear to have primary
    # stress several syllables too far left.
    stringr::str_replace_all(stringr::fixed("ˌ"), ".ˌ") |>
    stringr::str_replace_all(stringr::fixed("ˈ"), ".ˈ") |>
    stringr::str_replace_all(stringr::fixed(".."), ".") |>
    stringr::str_remove("^\\.")

  parts <- stringr::str_split(x, stringr::fixed("."))
  vapply(parts, function(syl) {
    hit <- which(stringr::str_detect(syl, stringr::fixed("ˈ")))
    if (length(hit)) hit[[1]] else 1L
  }, integer(1))
}

stressed_mid <- function(x) {
  stressed <- ifelse(
    stringr::str_detect(x, stringr::fixed("ˈ")),
    stringr::str_replace(x, "^.*ˈ", ""),
    x
  )
  out <- stringr::str_extract(stressed, "[eɛoɔ]")
  ifelse(is.na(out), "none", out)
}

# A deliberately coarse consonant comparison.  It ignores only alternations
# that the package derives in narrow_pt_vec(): affrication, rhotic realization,
# coda-l vocalization, nasal-vowel realization, and coda-s realization.
consonant_skeleton <- function(x) {
  x <- stringi::stri_trans_nfd(x) |>
    stringr::str_remove_all("^/|/$|[.ˈˌ()\\[\\]ːˑ̃͜͡]") |>
    stringr::str_replace_all("ɡ", "g") |>
    stringr::str_replace_all("tʃ", "t") |>
    stringr::str_replace_all("dʒ", "d") |>
    stringr::str_replace_all("[ɾʁɻɹh]", "R") |>
    stringr::str_replace_all("[rx]", "R") |>
    stringr::str_replace_all("l(?=$|[^aeiouɛɔɐɨɪʊ])", "w") |>
    stringr::str_replace_all("([aeiouɛɔɐɨɪʊ])[mn](?=$|[^aeiouɛɔɐɨɪʊ])", "\\1") |>
    stringr::str_replace_all("[szʃʒ](?=$|[^aeiouɛɔɐɨɪʊ])", "S") |>
    stringr::str_remove_all("[aeiouɛɔɐɨɪʊəjw]")
  x
}

# Whole-form comparison used in addition to the consonant skeleton.  It keeps
# the segmental sequence while neutralising only predictable broad/narrow
# differences: vowel reduction, nasal-vowel realization, affrication,
# rhotics, coda-l vocalization, and coda-s realization.  This prevents a
# stress-only repair from legitimising a still-wrong loanword transcription.
comparison_signature <- function(x) {
  x <- stringi::stri_trans_nfd(x) |>
    stringr::str_remove_all("^/|/$") |>
    stringr::str_replace_all(stringr::fixed("ˌ"), ".ˌ") |>
    stringr::str_replace_all(stringr::fixed("ˈ"), ".ˈ") |>
    stringr::str_replace_all(stringr::fixed(".."), ".") |>
    stringr::str_replace_all("ɡ", "g") |>
    stringr::str_replace_all("t͡?ʃ", "t") |>
    stringr::str_replace_all("d͡?ʒ", "d") |>
    # Collapse phonetic nasal vowels and their nasal offglides.
    stringr::str_replace_all(
      "([aeiouɛɔɐɨɪʊ])̃(?:[jw]̃?)?",
      "\\1"
    ) |>
    # The package writes many nasal codas segmentally in broad output.
    stringr::str_replace_all(
      "(?<=[aeiouɛɔ])[mn](?=\\.|$|[^aeiouɛɔ])",
      ""
    ) |>
    stringr::str_replace_all("[ɾʁɻɹh]", "R") |>
    stringr::str_replace_all("[rx]", "R") |>
    stringr::str_replace_all("l(?=\\.|$|[^aeiouɛɔɐɨɪʊ])", "W") |>
    stringr::str_replace_all("w", "W") |>
    stringr::str_replace_all("[szʃʒ](?=\\.|$|[^aeiouɛɔɐɨɪʊ])", "S") |>
    # Coarse vowel classes deliberately ignore unstressed reduction. Stressed
    # e/epsilon/o/open-o quality is checked separately and exactly.
    stringr::str_replace_all("[aɐ]", "A") |>
    stringr::str_replace_all("[eɛiɨɪə]", "E") |>
    stringr::str_replace_all("[oɔuʊ]", "O") |>
    stringr::str_remove_all("[.ˈˌ()\\[\\]ːˑ̃͜]")
  x
}

transcribe_pipeline <- function(x) {
  x |>
    transcribe_pt_vec() |>
    syllabify_pt_vec() |>
    stress_pt_vec() |>
    stringr::str_remove_all("\\.$") |>
    posttonic_pt_vec(ortho = x)
}

diacritized_candidates <- function(word) {
  chars <- stringr::str_split(word, "", simplify = TRUE)
  choices <- list(
    a = "á", e = c("é", "ê"), i = "í", o = c("ó", "ô"), u = "ú"
  )
  out <- word
  for (i in seq_along(chars)) {
    repl <- choices[[chars[[i]]]]
    if (is.null(repl)) next
    for (r in repl) {
      changed <- chars
      changed[[i]] <- r
      out <- c(out, paste0(changed, collapse = ""))
    }
  }
  unique(out)
}

# The maintainer-authored baseline is stored separately from generated,
# Wiktionary-derived corrections. This makes rebuilds idempotent and keeps the
# licensing/provenance boundary explicit.
manual_fix_tbl <- readr::read_tsv(
  "data-raw/pt_manual_lex_fix.tsv",
  show_col_types = FALSE,
  quote = ""
)
manual_fix <- stats::setNames(manual_fix_tbl$value, manual_fix_tbl$word)

old_pt_fix <- Fonology:::.get_fix_lex("pt_lex_user")
old_pt_ipa_fix <- Fonology:::.get_fix_lex("pt_ipa_lex")
old_pt_user <- Fonology:::.get_user_lex("pt_lex_user")
old_pt_ipa_user <- Fonology:::.get_user_lex("pt_ipa_lex")
on.exit({
  Fonology:::.set_fix_lex("pt_lex_user", old_pt_fix)
  Fonology:::.set_fix_lex("pt_ipa_lex", old_pt_ipa_fix)
  Fonology:::.set_user_lex("pt_lex_user", old_pt_user)
  Fonology:::.set_user_lex("pt_ipa_lex", old_pt_ipa_user)
}, add = TRUE)

Fonology:::.set_fix_lex("pt_lex_user", manual_fix)
Fonology:::.set_fix_lex("pt_ipa_lex", old_pt_ipa_fix)
Fonology:::.set_user_lex("pt_lex_user", stats::setNames(character(), character()))
Fonology:::.set_user_lex("pt_ipa_lex", stats::setNames(character(), character()))

pt_lex <- Fonology:::.get_pkg_data("pt_lex")
pt_freq <- Fonology:::.get_pkg_data("pt_freq") |>
  dplyr::transmute(
    word = stringr::str_to_lower(.data$word),
    freq = as.numeric(.data$freq)
  ) |>
  dplyr::filter(is.finite(.data$freq), .data$freq > 0)

source_summary <- raw |>
  dplyr::filter(!.data$word %in% stringr::str_to_lower(pt_lex$word)) |>
  dplyr::mutate(
    stress_source = stress_index(.data$ipa_source),
    mid_source = stressed_mid(.data$ipa_source),
    skeleton_source = consonant_skeleton(.data$ipa_source),
    signature_source = comparison_signature(.data$ipa_source)
  ) |>
  dplyr::group_by(.data$word) |>
  dplyr::summarise(
    pos = paste(sort(unique(.data$pos)), collapse = ","),
    ipa_source = paste(sort(unique(.data$ipa_source)), collapse = " | "),
    n_stress = dplyr::n_distinct(.data$stress_source),
    stress_source = dplyr::first(.data$stress_source),
    n_mid = dplyr::n_distinct(.data$mid_source),
    mid_source = dplyr::first(.data$mid_source),
    source_skeletons = paste(sort(unique(.data$skeleton_source)), collapse = "|"),
    source_signatures = paste(sort(unique(.data$signature_source)), collapse = "|"),
    .groups = "drop"
  ) |>
  dplyr::filter(.data$n_stress == 1L, .data$n_mid == 1L) |>
  dplyr::inner_join(pt_freq, by = "word")

current <- ipa(source_summary$word, lg = "pt") |>
  stringr::str_remove(stringr::fixed("*"))

candidates <- source_summary |>
  dplyr::mutate(
    current = current,
    stress_current = stress_index(.data$current),
    mid_current = stressed_mid(.data$current),
    correction_needed =
      .data$stress_current != .data$stress_source |
      (.data$mid_source != "none" & .data$mid_current != .data$mid_source)
  ) |>
  dplyr::filter(.data$correction_needed)

resolve_one <- function(
  word,
  stress_target,
  mid_target,
  source_skeletons,
  source_signatures
) {
  forms <- diacritized_candidates(word)
  outputs <- transcribe_pipeline(forms)
  stress <- stress_index(outputs)
  mid <- stressed_mid(outputs)
  skeleton <- consonant_skeleton(outputs)
  signature <- comparison_signature(outputs)
  source_set <- strsplit(source_skeletons, "|", fixed = TRUE)[[1]]
  signature_set <- strsplit(source_signatures, "|", fixed = TRUE)[[1]]

  ok <- stress == stress_target &
    (mid_target == "none" | mid == mid_target) &
    # A nasalized source vowel neutralizes the oral open/closed contrast. It
    # cannot license introducing an open-mid phoneme into broad output.
    (mid_target != "none" | !mid %in% c("ɛ", "ɔ")) &
    skeleton %in% source_set &
    signature %in% signature_set
  if (!any(ok)) return(c(form = NA_character_, corrected = NA_character_))

  forms <- forms[ok]
  outputs <- outputs[ok]
  # If nasalisation neutralises a source mid-vowel contrast, do not invent an
  # open-mid phoneme: prefer circumflex/closed-mid candidates over acute
  # e/o. Otherwise prefer the least marked candidate, then stable ordering.
  open_mid_penalty <- if (mid_target == "none") {
    stringr::str_detect(forms, "[éó]")
  } else {
    rep(FALSE, length(forms))
  }
  ord <- order(open_mid_penalty, nchar(forms), forms)
  c(form = forms[ord[[1]]], corrected = outputs[ord[[1]]])
}

resolved <- purrr::pmap_chr(
  candidates[c(
    "word", "stress_source", "mid_source", "source_skeletons",
    "source_signatures"
  )],
  function(
    word,
    stress_source,
    mid_source,
    source_skeletons,
    source_signatures
  ) {
    paste(
      resolve_one(
        word,
        stress_source,
        mid_source,
        source_skeletons,
        source_signatures
      ),
      collapse = "\t"
    )
  }
) |>
  stringr::str_split_fixed("\t", 2)

audit <- candidates |>
  dplyr::mutate(
    diacritized = resolved[, 1],
    corrected = resolved[, 2],
    resolvable = !is.na(.data$diacritized) & .data$diacritized != "NA",
    correction_class = dplyr::case_when(
      .data$stress_current != .data$stress_source &
        .data$mid_source != "none" & .data$mid_current != .data$mid_source ~
        "stress+mid",
      .data$stress_current != .data$stress_source ~ "stress",
      TRUE ~ "mid"
    )
  ) |>
  dplyr::filter(.data$resolvable) |>
  dplyr::arrange(dplyr::desc(.data$freq), .data$word) |>
  dplyr::mutate(
    correction_mass = .data$freq / sum(.data$freq),
    cumulative_mass = cumsum(.data$correction_mass),
    selected = dplyr::lag(.data$cumulative_mass, default = 0) < 0.95
  )

selected <- audit |>
  dplyr::filter(.data$selected)

if (any(!Fonology:::.is_pt_broad_ipa(selected$corrected))) {
  bad <- selected$word[!Fonology:::.is_pt_broad_ipa(selected$corrected)]
  stop("Generated non-broad Portuguese corrections: ", paste(bad, collapse = ", "))
}

readr::write_tsv(
  selected |>
    dplyr::transmute(
      word,
      mode = "diacritized",
      value = .data$diacritized,
      current,
      corrected,
      stress_source,
      mid_source,
      correction_class,
      pos,
      freq,
      ipa_source,
      source = source_url,
      license = "CC BY-SA 4.0"
    ),
  "data-raw/pt_wikt_corrections.tsv",
  quote = "none"
)

new_fix <- stats::setNames(selected$diacritized, selected$word)
pt_lex_fix <- c(manual_fix[!names(manual_fix) %in% names(new_fix)], new_fix)
pt_lex_fix <- pt_lex_fix[order(names(pt_lex_fix))]
save(pt_lex_fix, file = "data/pt_lex_fix.rda", compress = "xz")

message("Selected ", nrow(selected), " diacritized corrections.")
message(
  "Captured correction-bearing token mass: ",
  scales::percent(max(selected$cumulative_mass), accuracy = 0.1)
)
message("Wrote data-raw/pt_wikt_corrections.tsv and data/pt_lex_fix.rda")
