#' IPA transcriber for French
#'
#' Given a string, the function returns its IPA transcription with stress and syllabification.
#' @param word A possible string in French in its orthographic form
#' @return The phonemic transcription for the string in question
#' @noRd

ipa_fr <- function(word = "comportamento") {
  fr_ipa_lex <- .get_user_lex("fr_ipa_lex")

  wd <- stringr::str_to_lower(word) |>
    stringr::str_remove_all("[:punct:]")

  if (sum(stringr::str_detect(wd, pattern = "\\d")) > 0) {
    message("Input contains a number and will be ignored.")
    return(NA)
  }

  if (sum(stringr::str_detect(wd, pattern = "-")) > 0) {
    message("Input must be monomorphemic. Stress assignment may be incorrect.")
  }

  # IPA-override check: return stored IPA verbatim, bypassing the pipeline
  if (wd %in% names(fr_ipa_lex)) return(unname(fr_ipa_lex[wd]))

  wd <- wd |>
    transcribe_fr() |>
    syllabify_fr()

  return(wd)
}
