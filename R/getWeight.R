#' Syllable weight labeller
#'
#' Labels a given string according to its weight profile using Ls and Hs.
#' @param word The vector with strings of interest using IPA phonemic transcription, already syllabified and stressed
#' @param lg Language (defaults to Portuguese). Currently Portuguese, Spanish, French, Italian, and English are supported.
#' @return The sequence of Ls and Hs based on the word's weight profile given the phonology of the language in question
#' @examples
#' getWeight(word = c("kom.pu.ta.ˈdoɾ", "ka.ˈloɾ.pe"))
#' @export

getWeight <- function(word, lg = "Portuguese") {
  has_space <- stringr::str_detect(word, " ")

  if (any(has_space, na.rm = TRUE)) {
    message("Input must be a single word, phonemically transcribed and syllabified")
    return(NA)
  }

  if (stringr::str_to_lower(lg) %in% c("pt", "portuguese")) {
    output <- getWeight_pt(word)

    return(output)
  } else if (stringr::str_to_lower(lg) %in% c("sp", "spanish")) {
    output <- getWeight_sp(word)

    return(output)
  } else if (stringr::str_to_lower(lg) %in% c("fr", "french")) {
    output <- getWeight_fr(word)

    return(output)
  } else if (stringr::str_to_lower(lg) %in% c("it", "italian")) {
    output <- getWeight_it(word)

    return(output)
  } else if (stringr::str_to_lower(lg) %in% c("en", "english")) {
    output <- getWeight_en(word)

    return(output)
  } else {
    message("Only Portuguese, Spanish, French, Italian, and English are currently supported.")
    return(NA)
  }
}
