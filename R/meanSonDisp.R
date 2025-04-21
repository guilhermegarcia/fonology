#' Mean sonority dispersion calculator
#'
#' Given a word or a vector of words, the function returns
#' the mean sonority dispersion score based on the
#' sonority sequencing principle.
#' @param word A word in its orthographic (only for Portuguese) or phonemic form
#' @param phonemic Whether the input is phonemically transcribed (words in languages other than Portuguese must be set to \code{TRUE})
#' and syllabified. Default is \code{FALSE}
#' @return The mean sonority dispersion score
#' @examples
#' meanSonDisp(word = c("combrade", "prondo", "clauspricos"))
#' @export

meanSonDisp <- function(word = c(), phonemic = F) {
  if (phonemic) {
    d1 <- word |>
      demi() |>
      lapply(sonDisp) |>
      unlist()

    d2 <- word |>
      demi(d = 2) |>
      lapply(sonDisp) |>
      unlist()

    meanD <- mean(c(d1, d2))
    return(round(meanD, 2))
  }

  d1 <- word |>
    ipa_pt_vec() |>
    demi() |>
    lapply(sonDisp) |>
    unlist()

  d2 <- word |>
    ipa_pt_vec() |>
    demi(d = 2) |>
    lapply(sonDisp) |>
    unlist()

  meanD <- mean(c(d1, d2))

  return(round(meanD, 2))
}
