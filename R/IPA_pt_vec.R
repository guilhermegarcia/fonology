#' Vectorized IPA transcriber for Portuguese
#'
#' Given a string, the function returns its IPA transcription with stress and syllabification.
#' @param word A possible string in Portuguese in its orthographic form
#' @return The phonemic transcription for the string in question
#' @examples
#' ipa_pt_vec(word = c("palado", "antedom"));
#' @importFrom magrittr %>%
#' @export

ipa_pt_vec = function(word = c("palavra")){

  wd = stringr::str_to_lower(word) %>%
    stringr::str_remove_all(pattern = "[:punct:]")

  wd[stringr::str_detect(wd, "\\d")] = NA

  # Real words:
  # real = wd[wd %in% pt_lex$word]
  #
  # real = pt_lex %>%
  #   dplyr::filter(word %in% real) %>%
  #   dplyr::pull(pro)

  # Potentially nonce words:
  # nonce = wd[!wd %in% pt_lex$word]

  wd = wd %>%
    transcribe_pt_vec() %>%
    syllabify_pt_vec() %>%
    stress_pt_vec()

  # Both sets:
  # all = c(real, nonce)

  return(wd)
}

