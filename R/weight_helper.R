# Internal helper for syllable weight across languages.

.build_vowel_pattern <- function(vowels) {
  vowels <- vowels[order(nchar(vowels), decreasing = TRUE)]
  paste0("(", paste(vowels, collapse = "|"), ")$")
}

.getWeight_by_vowels <- function(word, vowels) {
  vowels_pattern <- .build_vowel_pattern(vowels)

  syl_list <- word |>
    stringr::str_split("\\.")

  syl_list <- lapply(syl_list, function(syls) {
    if (length(syls) == 1 && is.na(syls)) {
      return(NA_character_)
    }

    sapply(syls, function(syl) {
      if (is.na(syl)) return(NA_character_)

      syl_clean <- stringr::str_remove_all(syl, "[\u02c8\u02cc]")

      if (stringr::str_detect(syl_clean, vowels_pattern)) {
        "L"
      } else {
        "H"
      }
    }, USE.NAMES = FALSE)
  })

  profile <- lapply(syl_list, function(x) rev(rev(x)[1:3]))
  profile <- lapply(profile, function(x) x[!is.na(x)])

  profile <- lapply(profile, function(x) stringr::str_c(x, collapse = "")) |>
    unlist()

  profile[profile %in% ""] <- NA

  return(profile)
}
