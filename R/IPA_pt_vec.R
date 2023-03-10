#' Vectorized IPA transcriber for Portuguese
#'
#' Given a string, the function returns its IPA transcription with stress and syllabification.
#' Stress is assigned on the basis of the Portuguese Stress Lexicon for existing words, or lexical regularities
#' and probabilistic distributions in certain cases for hypothetical words
#' @param word A possible string in Portuguese in its orthographic form
#' @return The phonemic transcription for the string in question
#' @examples
#' ipa_pt_simple(word = c("palado", "antedom"));
#' @importFrom magrittr %>%
#' @export

ipa_pt_simple = function(word = c("palavra")){

  wd = stringr::str_to_lower(word) %>%
    stringr::str_remove_all(pattern = "[:punct:]")

  wd[stringr::str_detect(wd, "\\d")] = NA

  # Real words:
  real = wd[wd %in% pt_lex$word]

  real = pt_lex %>%
    dplyr::filter(word %in% real) %>%
    dplyr::pull(pro)

  # Potentially nonce words:
  nonce = wd[!wd %in% pt_lex$word]

  nonce = nonce %>%
    transcribe_pt() %>%
    syllabify_pt() %>%
    stress_pt_simple()

  # Both sets:
  all = c(real, nonce)

  return(all)
}

