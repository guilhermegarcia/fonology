#' Function to depluralize existing words
#'
#' Extracts the singular form of a plural word
#' @param word A word in its orthographic form
#' @return The singular form of the word
#' @examples
#' dePlu_pt(word = "variedades")

dePlu_pt = function(word = ""){
  if(!require("pacman", quietly = T)){install.packages("pacman")}
  pacman::p_load(tidyverse)

  word = str_to_lower(word)
  if(str_detect(word, pattern = "s$")){
    temp = str_remove(word, pattern = "s$")
    if(temp %in% pt_lex$word){
      return(temp)
    } else {
      return(word)
    }
  } else {
    return(word)
  }

}
