#' IPA transcriber for Spanish
#'
#' Given a string, the function returns its IPA transcription with stress and syllabification
#' @param word A possible string in Portuguese in its orthographic form
#' @return The phonemic transcription for the string in question
#' @noRd

ipa_sp = function(word = "comportamento"){

  wd = stringr::str_to_lower(word)

  if(stringr::str_detect(wd, pattern = "\\d")){
    message("Input contains a number and will be ignored.")
    return(NA)
  }

  if(stringr::str_detect(wd, pattern = "-")){
    message("Input must be monomorphemic. Stress assignment may be incorrect.")
  }

  wd = wd |>
    stringr::str_remove_all("[:punct:]")

  wd = wd |>
    transcribe_sp() |>
    syllabify_sp() |>
    stress_sp()

  return(wd)
}
