#' Syllable weight labeller for French
#'
#' Labels a given string in French according to its weight profile using Ls and Hs.
#' @param word The vector with strings of interest using IPA phonemic transcription, already syllabified and stressed
#' @return The sequence of Ls and Hs based on the word's weight profile given the phonology of French
#' @noRd

getWeight_fr <- function(word = c("\u025b\u0303.f\u0254\u0281.ma.tik")) {
  vowels <- get("vowels_fr", envir = as.environment("package:Fonology"))
  .getWeight_by_vowels(word, vowels)
}
