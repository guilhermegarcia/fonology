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


biGram_pt = function(word = c("")){
  word = word %>%
    stringr::str_replace_all(string = "d\u0361\u0292",
                             replacement = "d")

  word = word %>%
    stringr::str_replace_all(string = "t\u0361\u0283",
                             replacement = "t")


  purrr::map(word, ~biGram_pt_helper(word = .)) %>%
    unlist() %>%
    return()
}
