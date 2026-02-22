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

  # ── Strategy A: words with orthographic accent marks ──────────────────────
  # Italian uses grave/acute accents (à è é ì ò ó ù) to signal final stress
  # (and occasionally to disambiguate monosyllables). The stressed syllable is
  # the one containing the accented vowel.

  # à è é ê ì ò ó ô ù
  # ê (U+00EA) and ô (U+00F4) encode close mid-vowels; é/è and ó/ò encode open mid-vowels.
  accent_chars <- "\u00e0\u00e8\u00e9\u00ea\u00ec\u00f2\u00f3\u00f4\u00f9"
  which_diacritics <- stringr::str_detect(word, paste0("[", accent_chars, "]"))

  diacritics <- stringr::str_replace_all(
    string = word[which_diacritics],
    pattern = paste0("(\\w*[", accent_chars, "]\\w*)"),
    replacement = "\u02c8\\1"
  ) |>
    # Mid-vowel quality: open vs close distinction
    stringr::str_replace_all("\u00e9", "\u025b") |>   # é → ɛ (open-mid e)
    stringr::str_replace_all("\u00e8", "\u025b") |>   # è → ɛ (open-mid e, Italian convention)
    stringr::str_replace_all("\u00ea", "e") |>         # ê → e (close-mid e)
    stringr::str_replace_all("\u00f3", "\u0254") |>   # ó → ɔ (open-mid o)
    stringr::str_replace_all("\u00f2", "\u0254") |>   # ò → ɔ (open-mid o, Italian convention)
    stringr::str_replace_all("\u00f4", "o") |>         # ô → o (close-mid o)
    # Remaining stress-only diacritics → base vowel
    stringr::str_replace_all("\u00e0", "a") |>         # à → a
    stringr::str_replace_all("\u00ec", "i") |>         # ì → i
    stringr::str_replace_all("\u00f9", "u")

  names(diacritics) <- names(word[which_diacritics])
  word <- word[!which_diacritics]

  # ── Strategy B: monosyllabic words ────────────────────────────────────────
  which_monos <- stringr::str_detect(word, "\\.", negate = TRUE)

  monos <- stringr::str_replace_all(
    string = word[which_monos],
    pattern = "^(.*)$",
    replacement = "\u02c8\\1"
  )

  names(monos) <- names(word[which_monos])
  word <- word[!which_monos]

  # ── Strategy C: default penultimate stress ────────────────────────────────
  penults <- stringr::str_replace_all(
    string = word,
    pattern = "(\\w+\\.)(\\w+$)",
    replacement = "\u02c8\\1\\2"
  )
  names(penults) <- names(word)

  # ── Reassemble and restore original order ─────────────────────────────────
  output <- c(monos, penults, diacritics)
  output <- output[format(sort(as.numeric(names(output))), scientific = FALSE, trim = TRUE)]

  # ── Remove duplicate stress markers (keep rightmost) ──────────────────────
  output <- output |>
    stringr::str_replace("\u02c8(?=.*\u02c8)", "") |>
    stringr::str_replace("\u02c8(?=.*\u02c8)", "") |>
    stringr::str_replace("\u02c8(?=.*\u02c8)", "")

  # ── Post-stress glide formation ────────────────────────────────────────────
  # Unstressed i/u adjacent to another vowel become glides after stress is placed.
  output <- stringr::str_replace_all(output, "u([aeiou])", "w\\1")
  output <- stringr::str_replace_all(output, "([aeou])i([^aeiou]|$)", "\\1j\\2")

  # ── Remove double dots ─────────────────────────────────────────────────────
  output <- stringr::str_replace_all(output, "\\.\\.", ".")

  return(output)
}
