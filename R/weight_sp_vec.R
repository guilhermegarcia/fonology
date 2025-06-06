#' Syllable weight labeller for Spanish
#'
#' Labels a given string in Spanish according to its weight profile using Ls and Hs.
#' @param word The vector with strings of interest using IPA phonemic transcription, already syllabified and stressed
#' @return The sequence of Ls and Hs based on the word's weight profile given the phonology of Spanish
#' @noRd

getWeight_sp <- function(word = c("o\u027e.de.na.\u02c8do\u027e")) {
  syl_list <- word |>
    stringr::str_split("\\.")

  syl_list <- lapply(syl_list, function(x) {
    stringr::str_replace_all(x,
      pattern = "\\w+[jwlmn\u027espbtdkg\u027ezfv\u0283\u0292\u028e\u0272]",
      replacement = "H"
    )
  })

  syl_list <- lapply(syl_list, function(x) {
    stringr::str_replace_all(x,
      pattern = "[\\w*]{0,3}[\u00e3\u00f5aeiou\u025b\u0254]$",
      replacement = "L"
    )
  })

  syl_list <- lapply(syl_list, function(x) {
    stringr::str_replace_all(x,
      pattern = "H\u0303",
      replacement = "H"
    )
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
