#' Stress assigner to Spanish words
#'
#' Assigns stress to a given string.
#' @param word The string of interest using IPA phonemic transcription and already syllabified
#' @noRd
#' @return The stressed version of the string in question

stress_sp <- function(word) {
  # Assign names:
  names(word) <- seq(1:length(word))

  # Diacritics: replace with stress and get ride of accented vowels
  which_diacritics <- stringr::str_detect(string = word, pattern = "[\u00e1\u00e9\u00ed\u00f3\u00fa\u00e0\u00e8\u00ec\u00f2\u00f9\u00ea\u00f4\u00e2\u00f4\u00ea]", negate = F)
  diacritics <- stringr::str_replace_all(
    string = word[which_diacritics],
    pattern = "(\\w*[\u00e1\u00e9\u00ed\u00f3\u00fa\u00e0\u00e8\u00ec\u00f2\u00f9\u00ea\u00f4\u00e2\u00f4\u00ea]\\w*)",
    replacement = "\u02c8\\1"
  ) |>
    stringr::str_replace(
      pattern = "[\u00e1\u00e0]",
      replacement = "a"
    ) |>
    stringr::str_replace(
      pattern = "[\u00e9\u00e8]",
      replacement = "e"
    ) |>
    stringr::str_replace(
      pattern = "[\u00ed\u00ec]",
      replacement = "i"
    ) |>
    stringr::str_replace(
      pattern = "[\u00f3\u00f2]",
      replacement = "o"
    ) |>
    stringr::str_replace(
      pattern = "[\u00fa\u00f9]",
      replacement = "u"
    ) |>
    stringr::str_replace(
      pattern = "\u00ea",
      replacement = "e"
    ) |>
    stringr::str_replace(
      pattern = "\u00f4",
      replacement = "o"
    ) |>
    stringr::str_replace(
      pattern = "\u00e2",
      replacement = "a"
    ) |>
    stringr::str_replace(
      pattern = "\u00f4",
      replacement = "o"
    ) |>
    stringr::str_replace(
      pattern = "\u00ea",
      replacement = "e"
    )


  # Keep names for order:
  names(diacritics) <- names(word[which_diacritics])

  # Remove words with diacritics from initial vector:
  word <- word[!which_diacritics]

  # Monosyllabic word:
  which_monos <- stringr::str_detect(string = word, pattern = "\\.", negate = T)
  monos <- stringr::str_replace_all(
    string = word[stringr::str_detect(word, pattern = "\\.", negate = T)],
    pattern = "^(.*)$",
    replacement = "\u02c8\\1"
  )

  # Keep names for order:
  names(monos) <- names(word[which_monos])

  # Remove monos from initial vector:
  word <- word[!which_monos]

  # Word with final stress:
  which_heavy_finals <- stringr::str_detect(string = word, pattern = "\\.\\w+[p|b|t|d|k|g|z|f|v|\u0283|m|n|l|w|j|i|\u027e|u|r|\u00e3|\u00f5|w\u0303|j\u0303]$|\\.\\w+w\u0303s$|\\.\\w+j\u0303s$|\\.\\w+ns$")
  heavy_finals <- stringr::str_replace_all(
    string = word[which_heavy_finals],
    pattern = "\\.(\\w+[p|b|t|d|k|g|z|f|v|\u0283|m|n|l|\u027e|w|j|i|u|r|\u00e3|\u00f5|w\u0303|j\u0303|w\u0303s|j\u0303s])$",
    replacement = ".\u02c8\\1"
  )

  # Keep names for order:
  names(heavy_finals) <- names(word[which_heavy_finals])

  # Remove them:
  word <- word[!which_heavy_finals]

  # Else, penult stress:
  penults <- stringr::str_replace_all(
    string = word,
    pattern = "(\\w+\\.)(\\w+$)",
    replacement = "\u02c8\\1\\2"
  )
  # Keep names for order:
  names(penults) <- names(word)


  # Gather all words:
  output <- c(monos, heavy_finals, penults, diacritics)
  # names(output) = c(names(monos), names(mid_lows), names(heavy_finals), names(penults), names(diacritics))

  # Revert to original order:
  output <- output[as.character(sort(as.numeric(names(output))))]

  # Change stress to penult if word ends in am:
  output <- output |>
    stringr::str_replace(
      pattern = "(\\w+\\.)\u02c8(\\w*am$)",
      replacement = "\u02c8\\1\\2"
    )

  # Replace potential cases of multiple stresses:
  output <- output |>
    stringr::str_replace(pattern = "\u02c8(?=.*\u02c8)", replacement = "") |>
    stringr::str_replace(pattern = "\u02c8(?=.*\u02c8)", replacement = "") |>
    stringr::str_replace(pattern = "\u02c8(?=.*\u02c8)", replacement = "") |>
    stringr::str_replace(pattern = "\u02c8(?=.*\u02c8)", replacement = "") |>
    stringr::str_replace(pattern = "\u028ee\u027e$", replacement = "\u028e\u025b\u027e") |>
    stringr::str_replace(pattern = "a.o$", replacement = "aw") |>
    stringr::str_replace(pattern = "a.os$", replacement = "aws")

  # Adjustments post-stress:
  output <- output |>
    stringr::str_replace_all("(\\w*)(i)\\.\u02c8([aeiou])",
      replacement = "\u02c8\\1j\\3"
    )

  # Diphthongs/triphthongs:
  output <- stringr::str_replace_all(output, "u([aeoi])", "w\\1")
  output <- stringr::str_replace_all(output, "([aeou])[yi]", "\\1j")
  output <- stringr::str_replace_all(output, "i([ae\u00e9o\u00f3u])", "j\\1")
  output <- stringr::str_replace_all(output, "([aeoi])u", "\\1w")
  output <- stringr::str_replace_all(output, "[u\u00fc]([aeo\u00e1\u00e9\u00f3])[iy]", "w\\1j")
  output <- stringr::str_replace_all(output, "[i]([aeo\u00e1\u00e9\u00f3])[iy]", "j\\1j")
  output <- stringr::str_replace_all(output, "[iy]([aeo\u00e1\u00e9\u00f3])[u]", "j\\1w")
  output <- stringr::str_replace_all(output, "[u\u00fc]([aeo\u00e1\u00e9\u00f3])[u]", "w\\1w")
  # Paraguay words:
  output <- stringr::str_replace_all(output, "\\.(\\w*)u\\.\u02c8([aeoi])", "\\.\u02c8\\1w\\2")
  # Sociedad words:
  output <- stringr::str_replace_all(output, "\\.(\\w*)i\\.([aeo])", "\\.\\1j\\2")
  output <- stringr::str_replace_all(output, "\\.(\\w*)u\\.([aeo])", "\\.\\1w\\2")

  output <- stringr::str_replace_all(output, "i\u00f3n$", "jon")


  # Adjustments post-stress: words ending in -en] are verbs (paroxytones)
  output <- output |>
    stringr::str_replace_all("(\\w*)\\.\u02c8([^j]en$)",
      replacement = "\u02c8\\1.\\2"
    )

  # Remove double dots:
  output <- output |>
    stringr::str_replace_all("\\.\\.",
      replacement = "."
    )
  return(output)
}
