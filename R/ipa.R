#' IPA transcriber
#'
#' Given a string, the function returns its IPA transcription with stress and syllabification.
#' @param word A possible string in its orthographic form
#' @param narrow Boolean. Whether a narrow transcription is desired (default is \code{FALSE})
#' Narrow transcription is only available for Portuguese inputs and will be ignored for other languages
#' @param lg Language. Currently, Portuguese (\code{pt}), French (\code{fr}), Spanish (\code{sp}), or Italian (\code{it}) are supported
#' @return The phonemic transcription for the string in question
#' @examples
#' ipa(word = "palado")
#' @export

ipa <- function(word, lg = "Portuguese", narrow = FALSE) {
  word <- stringr::str_replace_all(word, "\\n", "")
  empty <- stringr::str_squish(word) == ""

  if (stringr::str_to_lower(lg) %in% c("pt", "portuguese")) {
    output <- ipa_pt_vec(word, narrow = narrow) |>
      stringr::str_replace_all("\\.\\s+", " ")
  } else if (stringr::str_to_lower(lg) %in% c("sp", "spanish")) {
    output <- ipa_sp(word) |>
      stringr::str_replace_all("\\.\\s+", " ")
  } else if (stringr::str_to_lower(lg) %in% c("fr", "french")) {
    output <- ipa_fr(word) |>
      stringr::str_replace_all("\\.\\s+", " ")
  } else if (stringr::str_to_lower(lg) %in% c("it", "italian")) {
    output <- ipa_it(word) |>
      stringr::str_replace_all("\\.\\s+", " ")
  } else {
    message("Only Portuguese, French, Spanish, and Italian are currently supported.")
    return(NA)
  }

  output[empty] <- ""
  return(output)
}
