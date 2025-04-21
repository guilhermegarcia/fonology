#' Vowel formant wrapper for \code{ggplot2}
#'
#' Reverses axes to accommodate \code{F1} and \code{F2} formants.
#' @return Axis reversal for both \code{x} and \code{y}
#' @examples
#' formants()
#' @export


formants <- function() {
  list(
    ggplot2::scale_y_reverse(position = "right"),
    ggplot2::scale_x_reverse(position = "top")
  )
}
