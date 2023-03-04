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
#' @importFrom magrittr %>%
#' @export

demi = function(word = c(), d = 1){

  word = word %>%
    stringr::str_split(pattern = "\\.") %>%
    unlist() %>%
    stringr::str_remove_all(pattern = "\'|\u02c8")

  d1 = word %>% stringr::str_extract(pattern = "^\\w*[aeiou\u025b\u0254\u00f8\u0251\u0259\u026a\u028a\u00e6\u0153\u025b\u0303\u0153\u0303\u0254\u0303]")
  d2 = word %>% stringr::str_extract(pattern = "[aeiou\u025b\u0254\u00f8\u0251\u0259\u026a\u028a\u00e6\u0153\u025b\u0303\u0153\u0303\u0254\u0303]\\w*$")

  if(d == 2){
    return(d2)
  } else if(d == 1){
    return(d1)
  } else {
    message("d must be 1 or 2")
    return(NA)
  }
}
