#' Text cleaner
#'
#' Given a string, the function removes punctuation, empty tokens, numbers, and normalizes lower case
#' @param text A possible string or text, in which case the function will first tokenize the text,
#' or a vector with words already tokenized, in which case tokenization is skipped
#' @return A vector with all words in the input, stripped of punctuation
#' @examples
#' cleanText(text = "Este é um texto em português? This is a text in English!");
#' @export

cleanText = function(text = ""){

  # Input is not tokenized yet:
  if(length(text) == 1){

    tokens = stringr::str_split(text, pattern = " ") |> unlist()

    # Empty cases:
    tokens = tokens[!tokens %in% ""]

    # Remove stress:
    tokens = tokens |>
      stringr::str_remove_all(pattern = "[\u02c8\u02cc]")

    # Numbers:
    tokens[stringr::str_detect(tokens, "\\d")] = NA

    # Punctuation:
    output = stringr::str_remove_all(tokens, "[:punct:]")

    # Case:
    output = stringr::str_to_lower(output)

    # Remove NAs:
    output = output[!is.na(output)]

    # Strip clitics:
    output = output |>
      strip_clitic_pt()

    return(output)
  } else {

    # Tokenized input:
    tokens = stringr::str_split(text, pattern = " ") |> unlist()
    # Remove stress:
    tokens = tokens |>
      stringr::str_remove_all(pattern = "[\u02c8\u02cc]")

    # Remove numbers:
    tokens = tokens |>
      stringr::str_remove_all(pattern = "\\d*")

    # Punctuation:
    output = stringr::str_remove_all(tokens, "[:punct:]")

    # Case:
    output = stringr::str_to_lower(output)

    # Strip clitics:
    output = output |>
      strip_clitic_pt()

    # Remove empty items:
    output = output[output != ""]

    return(output)

  }

}

