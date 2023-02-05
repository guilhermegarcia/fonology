#' Bigram probability for Portuguese
#'
#' Given a phonemically transcribed string, the function returns its bigram probability in log using the lexicon in the Portuguese Stress Lexicon as reference
#' @param word A possible string in Portuguese in its phonemic form without syllabification or stress. The only diacritic that should be used is the tilde for nasals, e.g., ã.
#' @return The phonemic transcription for the string in question
#' @examples
#' ipa_pt(word = "palado");
#' ipa_pt(word = "sɛtiko");
#' ipa_pt(word = "mãw̃");
#' @export

biGram_pt = function(word = ""){

  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(tidyverse)

  corpus = pt_lex %>%
    mutate(proB = str_remove_all(pro, "\\.|'")) %>%
    pull(proB)

  word_split = str_split(word, pattern = "")[[1]]

  bigrams = c()
  bigrams[1] = paste("^", word_split[1], sep = "")

  # Adding word-internal bigrams
  for(i in 1:(length(word_split)-1)){
    seq = str_c(word_split[i], word_split[i+1], sep = "")
    bigrams[length(bigrams)+1] = seq
  }

  # Adding word-final bigram
  bigrams[length(bigrams)+1] = str_c(word_split[length(word_split)], "$", sep = "")

  # Variable for all probabilities
  probs = c()

  for(bigram in bigrams){
    probs[length(probs)+1] = sum(str_count(words, bigram)) /
      sum(str_count(words, str_split(bigram, pattern = "")[[1]][1]))
  }

  return(log(prod(probs)))
}
