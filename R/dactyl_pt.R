#' Dactylic lowering helper function
#'
#' Given a string, the function tests if the string is LLL and has antepenult stress,
#' in which case the antepenultimate vowel height is checked and lowered if needed.
#' @param word A possible string in Portuguese in its phonemic form
#' @return The transcription with dactylic lowering if applicable
#' @examples
#' dact_pt(word = "ˈpe.te.le");
#' @export

dact_pt = function(word = ""){
  if(!require("pacman", quietly = T)){install.packages("pacman")}
  pacman::p_load(tidyverse)

  word = str_replace(word,
                     pattern = "(ˈ\\w*)e(\\.\\w+\\.\\w+$)",
                     replacement = "\\1ɛ\\2")

  word = str_replace(word,
                     pattern = "(ˈ\\w*)o(\\.\\w+\\.\\w+$)",
                     replacement = "\\1ɔ\\2")


  return(word)
}

dact_pt(word = c("ˈpe.te.le", "ˈko.te.lo"))
