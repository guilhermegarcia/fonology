#' Syllable weight labeller
#'
#' Labels a given string in Portuguese according to its weight profile using Ls and Hs.
#' Note that, although simple plural -s is taken into account, the function assumes the string provided is monomorphemic.
#' @param word The string of interest using IPA phonemic transcription, already syllabified and stressed
#' @return The sequence of Ls and Hs based on the word's weight profile given the phonology of Portuguese
#' @examples
#' getWeight_pt(word = "kom.pu.ta.ˈdoɾ");
#' @export

getWeight_pt = function(word = ""){

  if(!require("pacman", quietly = T)){install.packages("pacman")}
  pacman::p_load(tidyverse)

  word = str_to_lower(word)

  potentialPl = str_detect(word, "s$")
  sgWd = str_remove_all(string = word, pattern = "s$")




  # Remove stress
  word = str_remove_all(string = word,
                 pattern = "ˈ|'")


  # Light syllables
  word = str_replace_all(string = word,
                      pattern = "[\\w*]{0,3}[ãõaeiouɛɔ]$",
                      replacement = "L")

  word = str_replace_all(string = word,
                      pattern = "[\\w*]{0,3}[ãõaeiouɛɔ]\\.",
                      replacement = "L.")


  # Heavy syllables
  word = str_replace_all(string = word,
                      pattern = "\\w+[jwlmnɾspbtdkgɾzfvʃʒʎɲ]",
                      replacement = "H")

  # Remove syllabification
  word = str_remove_all(string = word,
                 pattern = "\\.")

  # Fix nasal diphthongs
  word = str_replace_all(string = word,
                        pattern = "H̃",
                        replacement = "H")

  # Pick only trisyllabic window
  word = str_sub(string = word,
                 start = -3L, end = -1L)

  # H -> L if s] = plural
  if(potentialPl & sgWd %in% pt_lex$pro){
   word = str_replace(string = word,
                      pattern = "H$",
                      replacement = "L")
  }


  return(word)

}
