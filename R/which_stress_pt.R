#' Stress labeller
#'
#' Labels a given stressed string as follows: Final, Penult, Antepenult
#' @param word The string of interest using IPA phonemic transcription, already syllabified and stressed
#' @param stress The symbol used to mark stress
#' @return The primary stress position
#' @examples
#' weight_pt(word = "kom.pu.ta.ˈdoɾ", stress = "ˈ");
#' @export

getStress = function(word = ""){
  if (!require("pacman", quietly = T)) install.packages("pacman")
  pacman::p_load(tidyverse)

  word = str_split(string = word,
                   pattern = "\\.") %>%
    unlist() %>%
    str_detect("ˈ")

  if(word[length(word)] == TRUE){
    return("Final")
  } else if(word[length(word)-1] == TRUE){
    return("Penult")
  } else if(word[length(word)-2] == TRUE){
    return("Antepenult")
  } else if(word[length(word)-3] == TRUE){
    return("Preantepenult")
  } else {
    return("Not a possible stress in Portuguese.")
  }

}

