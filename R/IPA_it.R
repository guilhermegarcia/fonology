#' IPA transcriber for Italian
#'
#' Given a string, the function returns its IPA transcription with stress and syllabification.
#' @param word A possible string in Italian in its orthographic form
#' @return The phonemic transcription for the string in question
#' @noRd

ipa_it <- function(word = "italiano") {
  wd <- stringr::str_to_lower(word) |>
    stringr::str_remove_all("[:punct:]")

  if (sum(stringr::str_detect(wd, pattern = "\\d")) > 0) {
    message("Input contains a number and will be ignored.")
    return(NA)
  }

  if (sum(stringr::str_detect(wd, pattern = "-")) > 0) {
    message("Input must be monomorphemic. Stress assignment may be incorrect.")
  }

  # Lexicon lookup (Option C): replace plain form with diacritized form so that
  # stress_it()'s diacritic rule places stress and vowel quality correctly.
  matches <- wd %in% names(it_lex)
  if (any(matches)) {
    wd[matches] <- it_lex[wd[matches]]
  }

  wd <- wd |>
    transcribe_it() |>
    syllabify_it() |>
    stress_it()

  return(wd)
}
