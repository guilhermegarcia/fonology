#' Text cleaner
#'
#' Given a string, the function removes punctuation, empty tokens, numbers, and normalizes lower case.
#' @param text A possible string or text
#' @return A vector with all words in the input, stripped of punctuation
#' @examples
#' cleanText(text = "Este é um texto em português? This is a text in English!");
#' @importFrom magrittr %>%
#' @export

cleanText = function(text = ""){

  tokens = stringr::str_split(text, pattern = " ") %>% unlist()

  # Empty cases:
  tokens = tokens[!tokens %in% ""]

  # Numbers:
  tokens[stringr::str_detect(tokens, "\\d")] = NA

  # Punctuation:
  output = stringr::str_remove_all(tokens, "[:punct:]")

  # Case:
  output = stringr::str_to_lower(output)

  # Remove NAs:
  output = output[!is.na(output)]

  return(output)

}
