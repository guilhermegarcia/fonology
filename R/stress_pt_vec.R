#' Vectorized stress assigner for Portuguese
#'
#' The function assigns regular to a vector of strings.
#' Stress is categorically defined, so the function is more simplistic than \code{stress_pt()}.
#' @param word A vector with possible strings in Portuguese, which must be phonemically transcribed and syllabified
#' @return The stressed strings
#' @noRd

stress_pt_vec <- function(word = c("ka.va.lo")) {
  # Assign names:
  names(word) <- seq(1:length(word))

  # Diacritics: replace with stress and get rid of accented vowels
  which_diacritics <- stringr::str_detect(string = word, pattern = "[\u00e1\u00e9\u00ed\u00f3\u00fa\u00e0\u00e8\u00ec\u00f2\u00f9\u00ea\u00f4\u00e2\u00f4\u00ea]", negate = F)
  diacritics <- stringr::str_replace_all(
    string = word[which_diacritics],
    pattern = "(\\w*[\u00e1\u00e9\u00ed\u00f3\u00fa\u00e0\u00e8\u00ec\u00f2\u00f9\u00ea\u00f4\u00e2\u00f4\u00ea]\\w*)",
    replacement = "\u02c8\\1"
  ) |>
    stringr::str_replace_all(
      pattern = "[\u00e1\u00e0]",
      replacement = "a"
    ) |>
    stringr::str_replace_all(
      pattern = "[\u00e9\u00e8]",
      replacement = "\u025b"
    ) |>
    stringr::str_replace_all(
      pattern = "[\u00ed\u00ec]",
      replacement = "i"
    ) |>
    stringr::str_replace_all(
      pattern = "[\u00f3\u00f2]",
      replacement = "\u0254"
    ) |>
    stringr::str_replace_all(
      pattern = "[\u00fa\u00f9]",
      replacement = "u"
    ) |>
    stringr::str_replace_all(
      pattern = "\u00ea",
      replacement = "e"
    ) |>
    stringr::str_replace_all(
      pattern = "\u00f4",
      replacement = "o"
    ) |>
    stringr::str_replace_all(
      pattern = "\u00e2",
      replacement = "a"
    ) |>
    stringr::str_replace_all(
      pattern = "\u00f4",
      replacement = "o"
    ) |>
    stringr::str_replace_all(
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

  # Word with mid-low V:
  which_mid_lows <- stringr::str_detect(string = word, pattern = "[\u0254\u025b]")
  mid_lows <- stringr::str_replace_all(
    string = word[stringr::str_detect(word, pattern = "[\u0254\u025b]")],
    pattern = "([:alpha:]*[\u0254\u025b])",
    replacement = "\u02c8\\1"
  )

  # Keep names for order:
  names(mid_lows) <- names(word[which_mid_lows])

  # Now remove mid-lows from initial vector:
  word <- word[!which_mid_lows]

  # Word with final stress:
  which_heavy_finals <- stringr::str_detect(string = word, pattern = "\\.\\w+[p|b|t|d|k|g|z|f|v|\u0283|m|n|l|w|j|i|\u027e|r|u|\u00e3|\u00f5|w\u0303|j\u0303]$|\\.\\w+w\u0303s$|\\.\\w+j\u0303s$|\\.\\w+ns$")
  heavy_finals <- stringr::str_replace_all(
    string = word[which_heavy_finals],
    pattern = "\\.(\\w+[p|b|t|d|k|g|z|f|v|\u0283|m|n|l|\u027e|r|w|j|i|u|\u00e3|\u00f5|w\u0303|j\u0303|w\u0303s|j\u0303s])$",
    replacement = ".\u02c8\\1"
  )

  # Keep names for order:
  names(heavy_finals) <- names(word[which_heavy_finals])

  # Remove them:
  word <- word[!which_heavy_finals]

  # Word with final stress (US IS or diph + S):
  which_heavy_finals2 <- stringr::str_detect(string = word, pattern = "\\.\\w+us$|\\.\\w+is$|\\w+js$|\\w+ws$|ens$")
  heavy_finals2 <- stringr::str_replace_all(
    string = word[which_heavy_finals2],
    pattern = "([:alpha:]+us$|[:alpha:]+is$|[:alpha:]+js$|[:alpha:]+ws|[:alpha:]ens$)$",
    replacement = "\u02c8\\1"
  )

  # Keep names for order:
  names(heavy_finals2) <- names(word[which_heavy_finals2])

  # Remove them:
  word <- word[!which_heavy_finals2]

  # Word with final stress (nasal diphthong + s):
  which_heavy_finals3 <- stringr::str_detect(string = word, pattern = "[nlr]s$|[wj]\u0303s$")
  heavy_finals3 <- stringr::str_replace_all(
    string = word[which_heavy_finals3],
    pattern = "([:alpha:]*[wj]\u0303[s])$",
    replacement = "\u02c8\\1"
  )

  # Keep names for order:
  names(heavy_finals3) <- names(word[which_heavy_finals3])

  # Remove them:
  word <- word[!which_heavy_finals3]

  # Else, penult stress:
  penults <- stringr::str_replace_all(
    string = word,
    pattern = "(\\w+\\.)(\\w+$)",
    replacement = "\u02c8\\1\\2"
  )
  # Keep names for order:
  names(penults) <- names(word)


  # Gather all words:
  output <- c(monos, mid_lows, heavy_finals, heavy_finals2, heavy_finals3, penults, diacritics)
  # names(output) = c(names(monos), names(mid_lows), names(heavy_finals), names(penults), names(diacritics))

  # Revert to original order:
  output <- output[as.character(sort(as.numeric(names(output))))]

  # Fix vowel height in Vl] sequences:
  output <- output |>
    stringr::str_replace(
      pattern = "(\u02c8\\w*)ol$",
      replacement = "\\1\u0254l"
    ) |>
    stringr::str_replace(
      pattern = "(\u02c8\\w*)el$",
      replacement = "\\1\u025bl"
    )

  # Fix words ending in -gem or -gens
  output <- output |>
    stringr::str_replace(
      pattern = "([:alpha:]+)\\.\u02c8(\u0292em)$$",
      replacement = "\u02c8\\1.\\2"
    ) |>
    stringr::str_replace(
      pattern = "([:alpha:]+)\\.\u02c8(\u0292ens)$$",
      replacement = "\u02c8\\1.\\2"
    )

  # Change stress to penult if word ends in am:
  output <- output |>
    stringr::str_replace(
      pattern = "(\\w+\\.)\u02c8(\\w*am$)",
      replacement = "\u02c8\\1\\2"
    )

  output <- output |>
    stringr::str_replace(
      pattern = "am$",
      replacement = "\u00e3w\u0303"
    )

  # Now replace z] with s]

  output <- output |>
    stringr::str_replace(
      pattern = "z$",
      replacement = "s"
    )

  # Replace potential cases of multiple stresses:
  output <- output |>
    stringr::str_replace(pattern = "\u02c8(?=.*\u02c8)", replacement = "") |>
    stringr::str_replace(pattern = "\u02c8(?=.*\u02c8)", replacement = "") |>
    stringr::str_replace(pattern = "\u02c8(?=.*\u02c8)", replacement = "") |>
    stringr::str_replace(pattern = "\u02c8(?=.*\u02c8)", replacement = "") |>
    stringr::str_replace(pattern = "\u028ee\u027e$", replacement = "\u028e\u025b\u027e") |>
    stringr::str_replace(pattern = "\u028eer$", replacement = "\u028e\u025br") |>
    stringr::str_replace(pattern = "a.o$", replacement = "aw") |>
    stringr::str_replace(pattern = "a.os$", replacement = "aws")

  # Adjustments to vowel quality when coda is nasal (diacritics):
  output <- output |>
    stringr::str_replace(
      pattern = "\u025b([mn])",
      replacement = "e\\1"
    )

  output <- output |>
    stringr::str_replace(
      pattern = "\u0254([mn])",
      replacement = "o\\1"
    )

  # NOTE: Adjustments to loanwords ending in rs
  # If penult is light, antepenult:
  output <- output |>
    stringr::str_replace_all(
      pattern = "(\\w+)\\.(\\w*[aeiou])\\.(\\w+[rl]s)$",
      replacement = "\u02c8\\1.\\2.\\3"
    )

  # NOTE: If penult is heavy and longer:
  output <- output |>
    stringr::str_replace_all(
      pattern = "(\\w+)\\.(\\w*[aeiou][bdfgklmnpkrstvz\u0283\u0292]+)\\.(\\w+[rl]s)$",
      replacement = "\\1.\u02c8\\2.\\3"
    )

  # Else, penult:
  output <- output |>
    stringr::str_replace_all(
      pattern = "^(\\w+)\\.(\\w+[rl]s)$",
      replacement = "\u02c8\\1.\\2"
    )

  output <- output |>
    stringr::str_replace_all(
      pattern = "bur.\u02c8ger$",
      replacement = "\u02c8bur.ger"
    )


  return(output)
}
