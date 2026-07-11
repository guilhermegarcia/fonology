#' Stress assigner for Italian words
#'
#' Assigns primary stress to a syllabified, phonemically transcribed Italian word.
#' Three strategies are applied in order: (1) orthographic accent marks determine
#' stress position; (2) monosyllables receive stress automatically; (3) all remaining
#' words default to penultimate stress.
#' @param word The string of interest using IPA phonemic transcription, already syllabified
#' @noRd
#' @return The stressed version of the string in question

stress_it <- function(word) {

  # Track original order for vectorized use
  names(word) <- seq(1:length(word))

  # Greek-type -ia endings surface as stressed hiatus, not a glide
  # (grafia, patia, nomia, scopia classes; cf. glide-final storia, grigia)
  word <- stringr::str_replace(word, "([ftmp])ja$", "\\1i.a")

  # \u2500\u2500 Strategy A: words with orthographic accent marks \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
  # Italian uses grave/acute accents (\u00e0 \u00e8 \u00e9 \u00ec \u00f2 \u00f3 \u00f9) to signal final stress
  # (and occasionally to disambiguate monosyllables). The stressed syllable is
  # the one containing the accented vowel.

  # Any diacritic on any vowel marks stress. For e/o the diacritic also encodes
  # vowel quality: \u00e8/\u00e9 \u2192 \u025b, \u00ea \u2192 e, \u00f2/\u00f3 \u2192 \u0254, \u00f4 \u2192 o. For a/i/u all diacritics
  # (grave, acute, circumflex) are treated as stress-only markers.
  accent_chars <- "\u00e0\u00e1\u00e2\u00e8\u00e9\u00ea\u00ec\u00ed\u00ee\u00f2\u00f3\u00f4\u00f9\u00fa\u00fb"
  which_diacritics <- stringr::str_detect(word, paste0("[", accent_chars, "]"))

  diacritics <- stringr::str_replace_all(
    string = word[which_diacritics],
    pattern = paste0("(\\w*[", accent_chars, "]\\w*)"),
    replacement = "\u02c8\\1"
  ) |>
    # Mid-vowel quality follows standard Italian orthography:
    # grave = open-mid (caff\u00e8, per\u00f2), acute/circumflex = close-mid (perch\u00e9)
    stringr::str_replace_all("\u00e8", "\u025b") |>   # \u00e8 \u2192 \u025b (open-mid e)
    stringr::str_replace_all("\u00e9", "e") |>         # \u00e9 \u2192 e (close-mid e)
    stringr::str_replace_all("\u00ea", "e") |>         # \u00ea \u2192 e (close-mid e)
    stringr::str_replace_all("\u00f2", "\u0254") |>   # \u00f2 \u2192 \u0254 (open-mid o)
    stringr::str_replace_all("\u00f3", "o") |>         # \u00f3 \u2192 o (close-mid o)
    stringr::str_replace_all("\u00f4", "o") |>         # \u00f4 \u2192 o (close-mid o)
    # Remaining stress-only diacritics \u2192 base vowel
    stringr::str_replace_all("\u00e0", "a") |>         # \u00e0 \u2192 a
    stringr::str_replace_all("\u00e1", "a") |>         # \u00e1 \u2192 a
    stringr::str_replace_all("\u00e2", "a") |>         # \u00e2 \u2192 a
    stringr::str_replace_all("\u00ec", "i") |>         # \u00ec \u2192 i
    stringr::str_replace_all("\u00ed", "i") |>         # \u00ed \u2192 i
    stringr::str_replace_all("\u00ee", "i") |>         # \u00ee \u2192 i
    stringr::str_replace_all("\u00f9", "u") |>         # \u00f9 \u2192 u
    stringr::str_replace_all("\u00fa", "u") |>         # \u00fa \u2192 u
    stringr::str_replace_all("\u00fb", "u")

  names(diacritics) <- names(word[which_diacritics])
  word <- word[!which_diacritics]

  # \u2500\u2500 Strategy B: monosyllabic words \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
  which_monos <- stringr::str_detect(word, "\\.", negate = TRUE)

  monos <- stringr::str_replace_all(
    string = word[which_monos],
    pattern = "^(.*)$",
    replacement = "\u02c8\\1"
  )

  names(monos) <- names(word[which_monos])
  word <- word[!which_monos]

  # \u2500\u2500 Strategy B1b: word-final falling diphthong attracts stress \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
  # (past remoto -ai, nouns like operai; sil.la.baj \u2192 sil.la.\u02c8baj)
  which_finalj <- stringr::str_detect(word, "[aeo]j$")

  finals <- stringr::str_replace(
    string = word[which_finalj],
    pattern = "([^.]+)$",
    replacement = "\u02c8\\1"
  )
  names(finals) <- names(word[which_finalj])
  word <- word[!which_finalj]

  # \u2500\u2500 Strategy B2: suffix classes with antepenultimate stress \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
  # Mined from the Wiktionary-derived lexicon (70-96% antepenult):
  # -ico/-ica/-ici/-iche, -ile/-ili, -ere, -ero, -olo/-ola/-oli/-ole,
  # -imo, -ono, -ine
  ante_pattern <- paste0(
    "(i\\.k[oaie]|i\\.t\u0283[ie]|i\\.l[ei]|e\\.re|e\\.ro|o\\.l[oaie]|",
    "i\\.mo|o\\.no|i\\.ne|[ei]\\.zi|d\u0292e\\.n[oai])$"
  )
  which_ante <- stringr::str_detect(word, ante_pattern) &
    stringr::str_count(word, stringr::fixed(".")) >= 2

  antes <- stringr::str_replace(
    string = word[which_ante],
    pattern = "(\\w+)\\.(\\w+\\.\\w+$)",
    replacement = "\u02c8\\1.\\2"
  )
  names(antes) <- names(word[which_ante])
  word <- word[!which_ante]

  # \u2500\u2500 Strategy C: default penultimate stress \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
  penults <- stringr::str_replace_all(
    string = word,
    pattern = "(\\w+\\.)(\\w+$)",
    replacement = "\u02c8\\1\\2"
  )
  names(penults) <- names(word)

  # \u2500\u2500 Reassemble and restore original order \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
  output <- c(monos, finals, antes, penults, diacritics)
  output <- output[format(sort(as.numeric(names(output))), scientific = FALSE, trim = TRUE)]

  # \u2500\u2500 Remove duplicate stress markers (keep rightmost) \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
  output <- output |>
    stringr::str_replace("\u02c8(?=.*\u02c8)", "") |>
    stringr::str_replace("\u02c8(?=.*\u02c8)", "") |>
    stringr::str_replace("\u02c8(?=.*\u02c8)", "")

  # \u2500\u2500 Post-stress glide formation \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
  # Unstressed i/u adjacent to another vowel become glides after stress is placed.
  output <- stringr::str_replace_all(output, "u([aeiou])", "w\\1")
  output <- stringr::str_replace_all(output, "([aeo])u([^aeiou]|$)", "\\1w\\2")
  output <- stringr::str_replace_all(output, "([aeou])i([^aeiou]|$)", "\\1j\\2")

  # \u2500\u2500 Stressed mid-vowel quality \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
  # Plain stressed e/o default to open-mid (62%/58% in the Wiktionary lexicon)
  # except in ending classes that are consistently close-mid (-mento, -one,
  # -ore, -oso, -etta, -ezza, -ese, -esco, -evole).
  keep_e <- stringr::str_detect(output, "(n\\.to|t\\.t[oaie]|t\\.ts[ae]|e\\.[zs]e|s\\.ko|o\\.l[ei]|men\\.te|s\\.s[ei]|s\\.te|e\\.v[ai]|e\\.te|\u0272\\.\u0272[oaie])$") &
    !stringr::str_detect(output, "n\\.tsa$")
  keep_o <- stringr::str_detect(output, "(o\\.n[eio]|o\\.re|o\\.r[ei]$|o\\.zo|o\\.z[aie]|r\\.so)$")

  # Diacritic-marked words have their quality resolved by the accent itself
  from_diacritics <- names(output) %in% names(diacritics)

  to_open_e <- !keep_e & !from_diacritics &
    stringr::str_detect(output, "\u02c8[^.]*e")
  to_open_o <- !keep_o & !from_diacritics &
    stringr::str_detect(output, "\u02c8[^.]*o")

  output[to_open_e] <- stringr::str_replace(
    output[to_open_e], "(\u02c8[^.]*)e", "\\1\u025b"
  )
  output[to_open_o] <- stringr::str_replace(
    output[to_open_o], "(\u02c8[^.]*)o", "\\1\u0254"
  )

  # \u2500\u2500 Remove double dots \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
  output <- stringr::str_replace_all(output, "\\.\\.", ".")

  return(output)
}
