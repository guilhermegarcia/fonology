#' Demi syllable extractor
#'
#' Given a string, the function returns its IPA transcription with stress and syllabification.
#' Stress is assigned on the basis of the Portuguese Stress Lexicon for existing words, or lexical regularities
#' and probabilistic distributions in certain cases for hypothetical words
#' @param word A syllabified and phonemically transcribed word; vectors are accepted
#' @param d Whether the function should return first (1) or second (2) demisyllable
#' @return The demisyllable of interest
#' @examples
#' demi(word = c("kram.pjo", "tlons.tri"), d = 1);
#' @export

demi = function(word = c(), d = 1){
  if(!require("pacman", quietly = T)){install.packages("pacman")}
  pacman::p_load(tidyverse)

  word = word %>%
    str_split(pattern = "\\.") %>%
    unlist() %>%
    str_remove_all(pattern = "'|ˈ")

  d1 = word %>% str_extract(pattern = "^\\w*[aeiouɛɔøɑəɪʊæœɛ̃œ̃ɔ̃]")
  d2 = word %>% str_extract(pattern = "[aeiouɛɔøɑəɪʊæœɛ̃œ̃ɔ̃]\\w*$")

  if(d == 2){
    return(d2)
  } else if(d == 1){
    return(d1)
  } else {
    message("d must be 1 or 2")
    return(NA)
  }
}