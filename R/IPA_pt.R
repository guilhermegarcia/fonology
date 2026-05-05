#' IPA transcriber for Portuguese
#'
#' Given a string, the function returns its IPA transcription with stress and
#' syllabification. Stress is assigned on the basis of the Portuguese Stress
#' Lexicon for existing words, or lexical regularities and probabilistic
#' distributions in certain cases for hypothetical words. Regex-derived
#' out-of-vocabulary forms are marked with \code{"*"}.
#' @param word A possible string in Portuguese in its orthographic form
#' @param narrow Boolean. Whether a narrow transcription is desired (defaults to \code{FALSE}).
#' @return The phonemic transcription for the string in question
#' @examples
#' ipa_pt(word = "palado")
#' @export

ipa_pt <- function(word = "palavra", narrow = FALSE) {
  if (any(stringr::str_detect(stringr::str_to_lower(word), pattern = "\\d"), na.rm = TRUE)) {
    message("Input contains a number and will be ignored.")
  }

  if (any(stringr::str_detect(stringr::str_to_lower(word), pattern = "-"), na.rm = TRUE)) {
    message("Input must be monomorphemic. The function will remove any clitics it detects.")
  }

  ipa_pt_vec(word, narrow = narrow)
}
