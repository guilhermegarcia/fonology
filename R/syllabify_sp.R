#' Syllabifier for Spanish
#'
#' Syllabifies a given word in Spanish.
#' @param word The string of interest using IPA phonemic transcription
#' @noRd
#' @return The syllabified version of the string in question

syllabify_sp <- function(word) {
  word <- stringr::str_replace_all(word, "([aeiou\u00e9\u00ed\u00e1\u00f3\u00fa])", "\\1.")
  word <- stringr::str_replace_all(word, "\\.([^aeiou\u00e9\u00ed\u00e1\u00f3]$)", "\\1")
  word <- stringr::str_remove_all(word, "\\.$")

  # Corrections:

  # Onset clusters:
  word <- stringr::str_replace_all(word, "\\.([pbtdkgsmn\u027e\u0272\u028el])([^aeiou\u00e9\u00ed\u00e1\u00f3jw])", "\\1.\\2")
  word <- stringr::str_replace_all(word, "([pbtdkg])\\.([l\u027e])", ".\\1\\2")
  word <- stringr::str_replace_all(word, "([t])\\.([\u0283])", ".t\u0361\u0283")
  word <- stringr::str_replace_all(word, "\\.([jw])([^aeiou\u00e9\u00ed\u00e1\u00f3])", "\\1.\\2")
  word <- stringr::str_replace_all(word, "\\.s([pbtdkgfv])", "s.\\1")
  word <- stringr::str_replace_all(word, "\\.([l])([g])", "\\1.\\2")

  return(word)
}
