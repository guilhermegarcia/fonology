#' Text cleaner
#'
#' Given a string, the function removes punctuation, empty tokens, numbers, and normalizes lower case.
#' @param text A possible string or text
#' @return A vector with all words in the input
#' @examples
#' cleanText(text = "Este é um texto em português? This is a text in English!");
#' @export

cleanText = function(text = ""){
  if (!require("pacman", quietly = T)){install.packages("pacman")}
  pacman::p_load(tidyverse)

  tokens = str_split(text, pattern = " ") %>% unlist()

  # Empty cases:
  tokens = tokens[!tokens %in% ""]

  # Numbers:
  tokens[str_detect(tokens, "\\d")] = NA

  # Punctuation:
  output = str_remove_all(tokens, "[:punct:]")

  # Case:
  output = str_to_lower(output)

  return(output)

}

