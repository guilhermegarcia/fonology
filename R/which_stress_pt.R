#' Stress labeller
#'
#' Labels a given stressed string as follows: Final, Penult, Antepenult.
#' Alternatively, extracts stressed syllable from input.
#' @param word The string of interest using IPA phonemic transcription, already syllabified and stressed. Syllable boundaries are always assumed to be indicated by a period.
#' @param stress The symbol used to mark stress
#' @param syl Whether the function should return the stressed syllable instead of the stress position (default is \code{FALSE})
#' @return The primary stress position
#' @examples
#' getStress(word = "kom.pu.ta.ˈdoɾ", stress = "ˈ")
#' getStress(word = "kom.pu.ta.ˈdoɾ", stress = "ˈ", syl = TRUE)
#' @export

getStress <- function(word = c("kom.pu.ta.\u02c8do\u027e"), stress = "\u02c8", syl = FALSE) {
  input_length <- length(word)
  if (length(which(stringr::str_detect(string = word, pattern = stress) == TRUE)) != input_length) {
    stop("No stress detected. Did you specify the correct diacritic for stress for?")
  }
  syl_list <- word |>
    stringr::str_split("\\.")

  indices <- lapply(syl_list, function(x) which(stringr::str_detect(rev(x), pattern = stress))) |> unlist()

  syllable <- purrr::map2_chr(
    syl_list,
    indices,
    ~ .x[length(.x) - .y + 1]
  ) |>
    stringr::str_remove(pattern = stress)

  indices <- stringr::str_replace_all(indices, pattern = "1", replacement = "final")
  indices <- stringr::str_replace_all(indices, pattern = "2", replacement = "penult")
  indices <- stringr::str_replace_all(indices, pattern = "3", replacement = "antepenult")
  indices <- stringr::str_replace_all(indices, pattern = "4", replacement = "pre-antepenult")
  indices <- stringr::str_replace_all(indices, pattern = "[56789]", replacement = "ungrammatical")

  if (syl) {
    return(syllable)
  } else {
    return(indices)
  }
}
