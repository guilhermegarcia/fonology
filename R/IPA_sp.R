#' IPA transcriber for Spanish
#'
#' Given a string, the function returns its IPA transcription with stress and syllabification.
#' @param word A possible string in Spanish in its orthographic form
#' @return The phonemic transcription for the string in question
#' @noRd

ipa_sp <- function(word = "comportamento") {
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
  if (wd %in% names(sp_ipa_lex)) return(unname(sp_ipa_lex[wd]))

  matches <- wd %in% names(sp_lex)
  if (any(matches)) wd[matches] <- sp_lex[wd[matches]]

  wd <- wd |>
    transcribe_sp() |>
    syllabify_sp() |>
    stress_sp()

  return(wd)
}
