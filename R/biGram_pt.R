#' Bigram probability for Portuguese
#'
#' Given a phonemically transcribed string, the function returns its bigram probability in log using the lexicon in the Portuguese Stress Lexicon as reference
#'
#' @param word A possible string in Portuguese in its phonemic form without syllabification or stress. The only diacritic that should be used is the tilde for nasals, e.g., ã.
#' @return The phonemic transcription for the string in question
#' @examples
#' biGram_pt(word = "paklode")
#' @importFrom magrittr %>%
#' @export

# biGram_pt = function(word = ""){
#
#   words = pt_lex %>%
#     dplyr::mutate(proB = stringr::str_remove_all(pro, "\\.|\'")) %>%
#     dplyr::pull(proB)
#
#   if(stringr::str_detect(string = word, pattern = "\\.|\'|\u02c8")){
#     stop("Input can\'t be syllabified/stressed.")
#   }
#   x1 = stringr::str_split(word, pattern = "")[[1]]
#
#   bigrams = c()
#   bigrams[1] = paste("^", x1[1], sep = "")
#
#   # Adding word-internal bigrams
#   for(i in 1:(length(x1)-1)){
#     seq = stringr::str_c(x1[i], x1[i+1], sep = "")
#     bigrams[length(bigrams)+1] = seq
#   }
#
#   # Adding word-final bigram
#   bigrams[length(bigrams)+1] = stringr::str_c(x1[length(x1)], "$", sep = "")
#
#   # Variable for all probabilities
#   probs = c()
#
#   for(bigram in bigrams){
#     probs[length(probs)+1] = sum(stringr::str_count(words, bigram)) /
#       sum(stringr::str_count(words, stringr::str_split(bigram, pattern = "")[[1]][1]))
#   }
#
#   for(i in 1:length(probs)){
#     if(is.nan(probs[i])){
#       stop("You are probably using narrow transcription. Bigrams are only calculated based on the phonemic inventory of Portuguese.")
#     }
#     if(probs[i] == 0){
#       probs[i] = 0.0000001
#     }
#   }
#
#
#   return(log(prod(probs)))
# }


# Updated, faster function:
biGram_pt = function(word = ""){

  if(stringr::str_detect(string = word, pattern = "[chqyw]")){
    message("Input most be phonemic, not orthographic.")
    return(NA)
  }
  word = word %>%
    stringr::str_remove_all("\\.|\u02c8") %>%
    stringr::str_c("^", ., "$") %>%
    stringr::str_split("") %>%
    unlist() %>%
    stringr::str_c(collapse = " ")

  bigramProb = ngram::ngram(str = word, n = 2) %>%
    ngram::get.phrasetable() %>%
    tibble::as_tibble() %>%
    tidyr::uncount(freq) %>%
    dplyr::mutate(ngrams = str_remove_all(ngrams, pattern = "\\s")) %>%
    dplyr::select(-c(prop)) %>%
    dplyr::left_join(bigrams_pt, by = "ngrams") %>%
    dplyr::filter(!ngrams %in% c("^^", "$$", "^$", "$^"))

  bigramProb[is.na(bigramProb$prop),]$prop = 1e-10

  bigramProb %>%
    dplyr::summarize(prob = log(prod(prop))) %>%
    dplyr::pull(prob) %>%
    return()

}

word = "plato"
