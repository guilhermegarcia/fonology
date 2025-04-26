#' Function to extract syllables from words
#'
#' Extracts a given syllable from syllabified strings in a vector.
#' @param word The strings of interest must be syllabified
#' @param pos The target syllable counting from the right edge of the word
#' @param from_right Whether to parse from the right (default is \code{TRUE}).
#' @param syl The symbol used for syllable boundaries (a period is used as the default)
#' @return The desired syllable if it exists. The function returns \code{NA} otherwise
#' @examples
#' getSyl(word = c("kom.pu.ta.dor", "pin.to.de"), pos = 2)
#' @export

getSyl <- function(word = c("pa.la.do"), pos = 1, from_right = TRUE, syl = "\\.") {
  syllables <- word |>
    stringr::str_remove_all(pattern = "\'|\u02c8|\u02cc") |>
    stringr::str_split(pattern = syl)

  if (!is.logical(from_right) || length(from_right) != 1) {
    stop("from_right requires a single TRUE or FALSE value.")
  }

  if (from_right) {
    output <- lapply(syllables, function(x) rev(x)[pos]) |>
      unlist()
  } else {
    output <- lapply(syllables, function(x) (x)[pos]) |>
      unlist()
  }
  return(output)
}
