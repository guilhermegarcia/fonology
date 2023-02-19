#' Function to extract syllables from words
#'
#' Extracts a given syllable from a syllabified string
#' @param word The string of interest must be syllabified
#' @param pos The target syllable counting from the right edge of the word
#' @param syl The symbol used for syllable boundaries (a period is used as the default)
#' @return The desired syllable if it exists
#' @examples
#' getSyl(word = "kom.pu.ta.dor", pos = 2);
#' getSyl(word = "kom-pu-ta-dor", pos = 3, syl = "-");
#' @export

getSyl = function(word = "", pos = 1, syl = "\\."){
  if(!require("pacman", quietly = T)){install.packages("pacman")}
  pacman::p_load(tidyverse)

  syllables = word %>%
    str_split(pattern = syl) %>%
    unlist() %>%
    rev() %>%
    str_remove_all(pattern = "['ˈˌ]")

  if(pos > length(syllables)){
    stop(paste("The word only has", length(syllables), "syllables."))
  }

  return(syllables[pos])

}
