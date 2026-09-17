#' Rewrite pipeline rhotics as phonemes
#'
#' Inside the Portuguese transcription pipeline the tap is written r and the
#' strong rhotic is written with a placeholder, U+0280, so that the two stay
#' distinct through syllabification and stress and neither is confused with an
#' orthographic x left over by the grapheme rules. This rewrites them as the
#' phonemic symbols used in the output: the tap as U+027E and the strong rhotic
#' as r. The order of the two replacements matters.
#'
#' @param x A character vector produced by the pipeline
#' @noRd
#' @return The vector with phonemic rhotics

phonemic_rhotics_pt <- function(x) {
  x <- stringr::str_replace_all(x, "r", "\u027e")
  stringr::str_replace_all(x, "\u0280", "r")
}
