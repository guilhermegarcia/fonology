#' Function to extract syllables from words
#'
#' Extracts a given syllable from syllabified strings in a vector.
#' @param word The strings of interest must be syllabified
#' @param pos The target syllable counting from the right edge of the word
#' @param dir The direction used, either \code{"from_right"} or \code{"from_left"} (default is \code{"from_right"})
#' @param syl The symbol used for syllable boundaries (a period is used as the default)
#' @return The desired syllable if it exists. The function returns \code{NA} otherwise
#' @examples
#' getSyl(word = c("kom.pu.ta.dor", "pin.to.de"), pos = 2)
#' @export

getSyl <- function(word = c("pa.la.do"), pos = 1, dir = "from_right", syl = "\\.") {
  syllables <- word |>
    stringr::str_remove_all(pattern = "\'|\u02c8|\u02cc") |>
    stringr::str_split(pattern = syl)

  if (dir == "from_left") {
    output <- lapply(syllables, function(x) (x)[pos]) |>
      unlist()
  } else if (dir == "from_right") {
    output <- lapply(syllables, function(x) rev(x)[pos]) |>
      unlist()
  } else {
    stop("Direction must be either 'from_left' or 'from_right'.")
  }

  return(output)
}
