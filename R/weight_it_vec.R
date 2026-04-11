#' Syllable weight labeller for Italian
#'
#' Labels a given string in Italian according to its weight profile using Ls and Hs.
#' @param word The vector with strings of interest using IPA phonemic transcription, already syllabified and stressed
#' @return The sequence of Ls and Hs based on the word's weight profile given the phonology of Italian
#' @noRd

getWeight_it <- function(word = c("i.ta.li.\u02c8a.no")) {
  vowels <- get("vowels_it", envir = as.environment("package:Fonology"))
  .getWeight_by_vowels(word, vowels)
}
