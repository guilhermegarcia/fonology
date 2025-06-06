#' Vectorized IPA transcriber for Portuguese
#'
#' Given a string, the function returns its IPA transcription with stress and syllabification.
#' @param word A possible string in Portuguese in its orthographic form
#' @param narrow Boolean. Whether a narrow transcription is desired (defaults to \code{FALSE})
#' @return The phonemic transcription for the string in question
#' @noRd

ipa_pt_vec <- function(word = c("palavra"), narrow = FALSE) {
  wd <- stringr::str_to_lower(word) |>
    stringr::str_remove_all(pattern = "[:punct:]") |>
    strip_clitic_pt()

  wd[stringr::str_detect(wd, "\\d")] <- NA

  wd <- wd |>
    transcribe_pt_vec() |>
    syllabify_pt_vec() |>
    stress_pt_vec() |>
    stringr::str_remove_all(pattern = "\\.$")


  # Check for narrow transcription:
  if (narrow == T) {
    wd <- wd |>
      narrow_pt_vec()

    return(wd)
  } else {
    return(wd)
  }
}
