#' Syllable weight labeller for French
#'
#' Labels a given string in French according to its weight profile using Ls and Hs.
#' @param word The vector with strings of interest using IPA phonemic transcription, already syllabified and stressed
#' @return The sequence of Ls and Hs based on the word's weight profile given the phonology of French
#' @noRd

getWeight_fr <- function(word = c("\u025b\u0303.f\u0254\u0281.ma.tik")) {
  vowels <- get("vowels_fr", envir = as.environment("package:Fonology"))
  heavy_vowels <- c(
    "\u0251", "e", "\u00f8", "o",
    "\u025b\u0303", "\u0153\u0303", "\u0254\u0303", "\u0251\u0303"
  )
  vowel_extract_pattern <- paste0(
    "(",
    paste(vowels[order(nchar(vowels), decreasing = TRUE)], collapse = "|"),
    ")"
  )
  vowels_pattern <- .build_vowel_pattern(vowels)

  syl_list <- word |>
    stringr::str_split("\\.")

  syl_list <- lapply(syl_list, function(syls) {
    if (length(syls) == 1 && is.na(syls)) {
      return(NA_character_)
    }

    n_syl <- length(syls)

    vapply(seq_along(syls), function(i) {
      syl <- syls[i]

      if (is.na(syl)) return(NA_character_)

      syl_clean <- stringr::str_remove_all(syl, "[\u02c8\u02cc]")
      nucleus <- stringr::str_extract(syl_clean, vowel_extract_pattern)

      if (is.na(nucleus)) {
        return(NA_character_)
      }

      has_coda <- !stringr::str_detect(syl_clean, vowels_pattern)
      open_final <- i == n_syl && !has_coda

      if (has_coda || (!open_final && nucleus %in% heavy_vowels)) {
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
