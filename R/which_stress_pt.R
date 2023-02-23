#' Stress labeller
#'
#' Labels a given stressed string as follows: Final, Penult, Antepenult
#' @param word The string of interest using IPA phonemic transcription, already syllabified and stressed
#' @param stress The symbol used to mark stress
#' @return The primary stress position
#' @examples
#' getStress(word = "kom.pu.ta.ˈdoɾ", stress = "ˈ");
#' @export

getStress = function(word = c("kom.pu.ta.ˈdoɾ"), stress = "ˈ"){
  if (!require("pacman", quietly = T)) install.packages("pacman")
  pacman::p_load(tidyverse)

  syl_list = word %>%
    str_split("\\.")

  indices = lapply(syl_list, function(x) which(str_detect(rev(x), pattern = stress))) %>% unlist()

  indices = str_replace_all(indices, pattern = "1", replacement = "final")
  indices = str_replace_all(indices, pattern = "2", replacement = "penult")
  indices = str_replace_all(indices, pattern = "3", replacement = "antepenult")
  indices = str_replace_all(indices, pattern = "4", replacement = "pre-antepenult")
  indices = str_replace_all(indices, pattern = "[56789]", replacement = "ungrammatical")

  return(indices)
}
