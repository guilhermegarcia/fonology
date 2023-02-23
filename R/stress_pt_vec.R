#' Vectorized stress assigner for Portuguese
#'
#' Given a string, the function assigns regular stress to a vector of strings
#' Stress is categorically defined, so the function is more simplistic than stress_pt().
#' and probabilistic distributions in certain cases for hypothetical words
#' @param word A vector with possible strings in Portuguese, which must be phonemically transcribed and syllabified
#' @return The stressed strings
#' @examples
#' stress_pt_simple(word = c("pa.la.do", "an.te.dom"));
#' @export

stress_pt_simple = function(word = c("ka.va.lo")){
  if(!require("pacman", quietly = T)){install.packages("pacman")}
  pacman::p_load(tidyverse)

  # Monosyllabic word:
  which_monos = str_detect(string = word, pattern = "\\.", negate = T)
  monos = str_replace_all(string = word[str_detect(word, pattern = "\\.", negate = T)],
                          pattern = "^(.*)$",
                          replacement = "'\\1")
  # Remove monos from initial vector:
  word = word[!which_monos]

  # Word with mid-low V:
  which_mid_lows = str_detect(string = word, pattern = "[ɔɛ]")
  mid_lows = str_replace_all(string = word[str_detect(word, pattern = "[ɔɛ]")],
                             pattern = "([:alpha:]*[ɔɛ])",
                             replacement = "'\\1")

  # Now remove mid-lows from initial vector:
  word = word[!which_mid_lows]

  # Word with final stress:
  which_heavy_finals = str_detect(string = word, pattern = "[pbtdkgszfvʃʒʎɲmnlɾwjiuãõww̃]$")
  heavy_finals = str_replace_all(string = word[str_detect(word, pattern = "[pbtdkgszfvʃʒʎɲmnlɾwjiuãõww̃]$")],
                                 pattern = "([:alpha:]+[pbtdkgszfvʃʒʎɲmnlɾwjiuãõw̃])$",
                                 replacement = "'\\1")

  # Remove them:
  word = word[!which_heavy_finals]

  # Else, penult stress:
  penults = str_replace_all(string = word,
                            pattern = "([:alpha:]+\\.)([:alpha:]+$)",
                            replacement = "'\\1\\2")


  output = c(monos, mid_lows, heavy_finals, penults)


  return(output)

}
