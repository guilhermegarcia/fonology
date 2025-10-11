#' Stress labeller
#'
#' Labels a given stressed string as follows: Final, Penult, Antepenult.
#' Alternatively, extracts stressed syllable from input.
#' @param word The string of interest using IPA phonemic transcription, already syllabified and stressed. Syllable boundaries are always assumed to be indicated by a period.
#' @param stress The symbol used to mark stress
#' @param syl Whether the function should return the stressed syllable instead of the stress position (default is \code{FALSE})
#' @return The primary stress position
#' @examples
#' getStress(word = "kom.pu.ta.\u02c8dor", stress = "ˈ")
#' getStress(word = "kom.pu.ta.\u02c8dor", stress = "ˈ", syl = TRUE)
#' @export

getStress <- function(word = c("kom.pu.ta.\u02c8dor"), stress = "\u02c8", syl = FALSE) {
  input_length <- length(word)

  # Handle NA input
  if (all(is.na(word))) {
    return(rep(NA_character_, input_length))
  }

  # Check which words have stress marks
  has_stress <- stringr::str_detect(string = word, pattern = stress)

  # Replace NAs in has_stress with FALSE
  has_stress[is.na(has_stress)] <- FALSE

  # Initialize output vectors
  indices <- rep(NA_character_, input_length)
  syllable <- rep(NA_character_, input_length)

  # Only process words that have stress marks
  if (any(has_stress)) {
    words_with_stress <- word[has_stress]
    syl_list <- stringr::str_split(words_with_stress, "\\.")

    # Find stress position - ensure we always get a result for each word
    stress_indices <- sapply(syl_list, function(x) {
      stress_pos <- which(stringr::str_detect(rev(x), pattern = stress))
      if (length(stress_pos) == 0) {
        return(NA_integer_)
      }
      return(stress_pos[1]) # Take first if multiple
    })

    # Extract syllables for words with valid stress positions
    valid_stress <- !is.na(stress_indices)
    if (any(valid_stress)) {
      syllable[has_stress][valid_stress] <- stringr::str_remove(
        purrr::map2_chr(
          syl_list[valid_stress],
          stress_indices[valid_stress],
          ~ .x[length(.x) - .y + 1]
        ),
        pattern = stress
      )
    }

    # Convert indices to labels
    stress_labels <- as.character(stress_indices)
    stress_labels <- stringr::str_replace_all(stress_labels, pattern = "^1$", replacement = "final")
    stress_labels <- stringr::str_replace_all(stress_labels, pattern = "^2$", replacement = "penult")
    stress_labels <- stringr::str_replace_all(stress_labels, pattern = "^3$", replacement = "antepenult")
    stress_labels <- stringr::str_replace_all(stress_labels, pattern = "^4$", replacement = "pre-antepenult")
    stress_labels <- stringr::str_replace_all(stress_labels, pattern = "^[56789]$", replacement = "ungrammatical")

    indices[has_stress] <- stress_labels
  }

  if (syl) {
    return(syllable)
  } else {
    return(indices)
  }
}
