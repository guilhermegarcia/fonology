#' Syllable counter
#'
#' The function counts the number of syllables in a given string
#' @param word A possible string that is already syllabified
#' @param sep The syllable boundary employed. By default, the
#' function assumes a period separates syllables, but you may want to
#' change that using the `sep` argument. Bear in mind that you must
#' follow the typical syntax used in RegEx in R for special symbols
#' @return The number of syllables in the string of interest
#' @examples
#' countSyl("kan.ˈto.zo")
#' @export

countSyl = function(word = "", sep = "\\."){
  count = stringr::str_count(word, pattern = sep) + 1

  return(count)
}
