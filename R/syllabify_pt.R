#' Syllabifier for Portuguese
#'
#' Returns syllabification for a given string
#' @param word The string of interest using IPA phonemic transcription
#' @return The syllabification for the string in question
#' @examples
#' syllabify_pt(word = "komputadoɾ");
#' @export

syllabify_pt = function(word = ""){

  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(tidyverse)

  # Start with CV:
  word = str_replace_all(string = word,
                      pattern = "([aeiouɛɔ])",
                      replacement = "\\1.")

  # Fix diphthongs:
  word = str_replace_all(string = word,
                      pattern = "([aeiouɛɔ])\\.([wj])",
                      replacement = "\\1\\2.")

  # Fix onset clusteres:
  word = str_replace_all(string = word,
                      pattern = "\\.([lmnɾskp])([pbtdkgsɾzfvʃʒʎɲmn])",
                      replacement = "\\1.\\2")

  # Remove empty final syllables:
  word = str_remove_all(string = word,
                     pattern = "\\.$")

  # Remove C-syllables word finally:
  word = str_replace_all(string = word,
                      pattern = "\\.([pbtdkgszfvʃʒʎlmnɾs])$",
                      replacement = "\\1")

  return(word)

}



