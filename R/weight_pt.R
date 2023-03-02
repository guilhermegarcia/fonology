#' Syllable weight labeller
#'
#' Labels a given string in Portuguese according to its weight profile using Ls and Hs.
#' Note that, although simple plural -s is taken into account, the function assumes the string provided is monomorphemic.
#' For a simplified but vectorized version of the function, use getWeight_pt_simple().
#' @param word The string of interest using IPA phonemic transcription, already syllabified and stressed
#' @return The sequence of Ls and Hs based on the word's weight profile given the phonology of Portuguese
#' @examples
#' getWeight_pt(word = "kom.pu.ta.ˈdoɾ");
#' @importFrom magrittr %>%
#' @export

getWeight_pt = function(word = "kom.pu.ta.ˈdoɾ"){

  word = stringr::str_to_lower(word)

  potentialPl = stringr::str_detect(word, "s$")
  sgWd = stringr::str_remove_all(string = word, pattern = "s$")

  # Remove stress
  word = stringr::str_remove_all(string = word,
                        pattern = "ˈ|'")

  # Light syllables
  word = stringr::str_replace_all(string = word,
                         pattern = "[\\w*]{0,3}[ãõaeiouɛɔ]$",
                         replacement = "L")

  word = stringr::str_replace_all(string = word,
                         pattern = "[\\w*]{0,3}[ãõaeiouɛɔ]\\.",
                         replacement = "L.")


  # Heavy syllables
  word = stringr::str_replace_all(string = word,
                         pattern = "\\w+[jwlmnɾspbtdkgɾzfvʃʒʎɲ]",
                         replacement = "H")

  # Remove syllabification
  word = stringr::str_remove_all(string = word,
                        pattern = "\\.")

  # Fix nasal diphthongs
  word = stringr::str_replace_all(string = word,
                         pattern = "H̃",
                         replacement = "H")

  # Pick only trisyllabic window
  word = stringr::str_sub(string = word,
                 start = -3L, end = -1L)

  # H -> L if s] = plural
  if(potentialPl & sgWd %in% pt_lex$pro){
    word = stringr::str_replace(string = word,
                       pattern = "H$",
                       replacement = "L")
  }


  return(word)

}
