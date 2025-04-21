#' IPA transcriber for Spanish
#'
#' Given a string, the function returns its IPA transcription with stress and syllabification.
#' @param word A possible string in Portuguese in its orthographic form
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

  wd <- wd |>
    transcribe_sp() |>
    syllabify_sp() |>
    stress_sp()

  return(wd)
}
