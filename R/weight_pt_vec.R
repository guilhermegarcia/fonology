#' Syllable weight labeller for Portuguese
#'
#' Labels a given string in Portuguese according to its weight profile using Ls and Hs.
#' @param word The vector with strings of interest using IPA phonemic transcription, already syllabified and stressed
#' @return The sequence of Ls and Hs based on the word's weight profile given the phonology of Portuguese
#' @noRd

getWeight_pt <- function(word = c("kom.pu.ta.\u02c8do\u027e")) {
  # IPA vowels (including nasal and mid-low vowels)
  vowels_pattern <- "[\u00e3\u00f5aeiou\u025b\u0254]"

  syl_list <- word |>
    stringr::str_split("\\.")

  # Classify each syllable as H (heavy, ends in consonant) or L (light, ends in vowel)
  syl_list <- lapply(syl_list, function(syls) {
    sapply(syls, function(syl) {
      # Remove stress markers for analysis
      syl_clean <- stringr::str_remove_all(syl, "[\u02c8\u02cc]")
      # Check if syllable ends in a vowel (light) or consonant (heavy)
      if (stringr::str_detect(syl_clean, paste0(vowels_pattern, "$"))) {
        return("L")
      } else {
        return("H")
      }
    }, USE.NAMES = FALSE)
  })

  # Select only trisyllabic window:
  profile <- lapply(syl_list, function(x) rev(rev(x)[1:3]))

  # Remove NAs:
  profile <- lapply(profile, function(x) x[!is.na(x)])

  # Collapse weight:
  profile <- lapply(profile, function(x) stringr::str_c(x, collapse = "")) |>
    unlist()

  profile[profile %in% ""] <- NA

  return(profile)
}
