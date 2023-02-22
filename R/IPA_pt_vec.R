#' Vectorized IPA transcriber for Portuguese
#'
#' Given a string, the function returns its IPA transcription with stress and syllabification.
#' Stress is assigned on the basis of the Portuguese Stress Lexicon for existing words, or lexical regularities
#' and probabilistic distributions in certain cases for hypothetical words
#' @param word A possible string in Portuguese in its orthographic form
#' @return The phonemic transcription for the string in question
#' @examples
#' ipa_pt_simple(word = c("palado", "antedom"));
#' @export

ipa_pt_simple = function(word = c("palavra")){
  if(!require("pacman", quietly = T)){install.packages("pacman")}
  pacman::p_load(tidyverse)

  wd = str_to_lower(word) %>%
    str_remove_all(pattern = "[:punct:]")

  # Real words:
  real = wd[wd %in% pt_lex$word]

  real = pt_lex %>%
    filter(word %in% real) %>%
    pull(pro)

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
