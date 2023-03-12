#' Vowel formant wrapper for ggplot2
#'
#' Reverses axes to accommodate F1 and F2 formants
#' @return Axis reversal for both x and y
#' @examples
#' formants();
#' @export


formants = function(){
  list(
    ggplot2::scale_y_reverse(position = "right"),
    ggplot2::scale_x_reverse(position = "top")
  )
}
