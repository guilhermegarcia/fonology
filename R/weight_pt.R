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

getWeight_pt = function(word = "kom.pu.ta.\u02c8do\u027e"){

  word = stringr::str_to_lower(word)

  # Remove stress
  word = stringr::str_remove_all(string = word,
                        pattern = "\u02c8|\'")

  # Light syllables
  word = stringr::str_replace_all(string = word,
                         pattern = "[\\w*]{0,3}[\u00e3\u00f5aeiou\u025b\u0254]$",
                         replacement = "L")

  word = stringr::str_replace_all(string = word,
                         pattern = "[\\w*]{0,3}[\u00e3\u00f5aeiou\u025b\u0254]\\.",
                         replacement = "L.")


  # Heavy syllables
  word = stringr::str_replace_all(string = word,
                         pattern = "\\w+[jwlmn\u027espbtdkg\u027ezfv\u0283\u0292\u028e\u0272]",
                         replacement = "H")

  word = stringr::str_replace_all(string = word,
                                  pattern = "\\w+[jwlmn\u027espbtdkg\u027ezfv\u0283\u0292\u028e\u0272]",
                                  replacement = "H")

  # Remove syllabification
  word = stringr::str_remove_all(string = word,
                        pattern = "\\.")

  # Fix nasal diphthongs
  word = stringr::str_replace_all(string = word,
                         pattern = "H\u0303",
                         replacement = "H")

  # Pick only trisyllabic window
  word = stringr::str_sub(string = word,
                 start = -3L, end = -1L)


  return(word)

}
