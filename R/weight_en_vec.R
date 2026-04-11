#' Syllable weight labeller for English
#'
#' Labels a given string in English according to its weight profile using Ls and Hs.
#' @param word The vector with strings of interest using IPA phonemic transcription, already syllabified and stressed
#' @return The sequence of Ls and Hs based on the word's weight profile given the phonology of English
#' @noRd

getWeight_en <- function(word = c("\u02c8h\u0251s.p\u026a.t\u0259l")) {
  syl_list <- word |>
    stringr::str_split("\\.")

  syl_list <- lapply(syl_list, function(syls) {
    if (length(syls) == 1 && is.na(syls)) {
      return(NA_character_)
    }

    vapply(syls, function(syl) {
      if (is.na(syl)) return(NA_character_)

      if (.is_heavy_syllable_ipa_en(syl)) {
        "H"
      } else {
        "L"
      }
    }, USE.NAMES = FALSE, FUN.VALUE = character(1))
  })

  profile <- lapply(syl_list, function(x) rev(rev(x)[1:3]))
  profile <- lapply(profile, function(x) x[!is.na(x)])

  profile <- lapply(profile, function(x) stringr::str_c(x, collapse = "")) |>
    unlist()

  profile[profile %in% ""] <- NA

  profile
}
