#' Bigram probability for Portuguese
#'
#' Given a phonemically transcribed string, the function returns its bigram probability in log using the lexicon in the Portuguese Stress Lexicon as reference
#' @param word A possible string in Portuguese in its phonemic form without syllabification or stress. The only diacritic that should be used is the tilde for nasals, e.g., ã.
#' @return The phonemic transcription for the string in question
#' @examples
#' biGram_pt(word = "paklode")
#' @export

biGram_pt = function(word = ""){

  if (!require("pacman", quietly = T)) install.packages("pacman")
  pacman::p_load(tidyverse)

  words = pt_lex %>%
    mutate(proB = str_remove_all(pro, "\\.|'")) %>%
    pull(proB)

  if(str_detect(string = word, pattern = "\\.|'|ˈ")){
    stop("Input can't be syllabified/stressed.")
  }
  x1 = str_split(word, pattern = "")[[1]]

  bigrams = c()
  bigrams[1] = paste("^", x1[1], sep = "")

  # Adding word-internal bigrams
  for(i in 1:(length(x1)-1)){
    seq = str_c(x1[i], x1[i+1], sep = "")
    bigrams[length(bigrams)+1] = seq
  }

  # Adding word-final bigram
  bigrams[length(bigrams)+1] = str_c(x1[length(x1)], "$", sep = "")

  # Variable for all probabilities
  probs = c()

  for(bigram in bigrams){
    probs[length(probs)+1] = sum(str_count(words, bigram)) /
      sum(str_count(words, str_split(bigram, pattern = "")[[1]][1]))
  }

  for(i in 1:length(probs)){
    if(is.nan(probs[i])){
      stop("You are probably using narrow transcription. Bigrams are only calculated based on the phonemic inventory of Portuguese.")
    }
    if(probs[i] == 0){
      probs[i] = 0.0000001
    }
  }


  return(log(prod(probs)))
}
