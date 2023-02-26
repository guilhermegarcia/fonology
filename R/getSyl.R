#' Function to extract syllables from words
#'
#' Extracts a given syllable from syllabified strings in a vector
#' @param word The strings of interest must be syllabified
#' @param pos The target syllable counting from the right edge of the word
#' @param syl The symbol used for syllable boundaries (a period is used as the default)
#' @return The desired syllable if it exists. The function returns NA otherwise
#' @examples
#' getSyl(word = c("kom.pu.ta.dor", "pin.to.de"), pos = 2);
#' @export

getSyl = function(word = c("pa.la.do"), pos = 1, syl = "\\."){
  if(!require("pacman", quietly = T)){install.packages("pacman")}
  pacman::p_load(tidyverse)

  syllables = word %>%
    str_remove_all(pattern = "'|ˈ|ˌ") %>%
    str_split(pattern = syl)

  output = lapply(syllables, function(x) rev(x)[pos]) %>%
    unlist()

  return(output)
}
