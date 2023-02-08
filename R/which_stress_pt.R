#' Stress labeller for Portuguese
#'
#' Labels a given stressed string as follows: Final, Penult, Antepenult
#' @param word The string of interest using IPA phonemic transcription, already syllabified and stressed
#' @return The sequence of Ls and Hs based on the word's weight profile given the phonology of Portuguese
#' @examples
#' weight_pt(word = "kom.pu.ta.ˈdoɾ");
#' @export

which_stress_pt = function(word = ""){
  if (!require("pacman")) install.packages("pacman")
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
  } else {
    return("Not a possible stress in Portuguese.")
  }

}

