#' Syllable weight labeller
#'
#' Labels a given string in Portuguese according to its weight profile using Ls and Hs.
#' @param word The vector with strings of interest using IPA phonemic transcription, already syllabified and stressed
#' @param lg Language (defaults to Portuguese)
#' @return The sequence of Ls and Hs based on the word's weight profile given the phonology of the language in question
#' @examples
#' getWeight(word = c("kom.pu.ta.ˈdoɾ", "ka.ˈloɾ.pe"))
#' @export

getWeight <- function(word, lg = "Portuguese") {
  if (stringr::str_to_lower(lg) %in% c("pt", "portuguese")) {
    output <- getWeight_pt(word)

    return(output)
  } else if (stringr::str_to_lower(lg) %in% c("sp", "spanish")) {
    output <- getWeight_sp(word)

    return(output)
  } else {
    message("Only Portuguese and Spanish are currently supported.")
    return(NA)
  }
}
