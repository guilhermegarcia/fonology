#' Vectorized stress assigner for Portuguese
#'
#' The function assigns regular to a vector of strings
#' Stress is categorically defined, so the function is more simplistic than stress_pt().
#' @param word A vector with possible strings in Portuguese, which must be phonemically transcribed and syllabified
#' @return The stressed strings
#' @examples
#' stress_pt_vec(word = c("pa.la.do", "an.te.dom"));
#' @importFrom magrittr %>%
#' @export

stress_pt_vec = function(word = c("ka.va.lo")){

  # Assign names:
  names(word) = seq(1:length(word))

  # Diacritics: replace with stress and get ride of accented vowels
  which_diacritics = stringr::str_detect(string = word, pattern = "[\u00e1\u00e9\u00ed\u00f3\u00fa\u00e0\u00e8\u00ec\u00f2\u00f9\u00ea\u00f4\u00e2\u00f4\u00ea]", negate = F)
  diacritics = stringr::str_replace_all(string = word[which_diacritics],
                                        pattern = "(\\w*[\u00e1\u00e9\u00ed\u00f3\u00fa\u00e0\u00e8\u00ec\u00f2\u00f9\u00ea\u00f4\u00e2\u00f4\u00ea]\\w*)",
                                        replacement = "\u02c8\\1") %>%
    stringr::str_replace(pattern = "[\u00e1\u00e0]",
                         replacement = "a") %>%
    stringr::str_replace(pattern = "[\u00e9\u00e8]",
                         replacement = "\u025b") %>%
    stringr::str_replace(pattern = "[\u00ed\u00ec]",
                         replacement = "i") %>%
    stringr::str_replace(pattern = "[\u00f3\u00f2]",
                         replacement = "\u0254") %>%
    stringr::str_replace(pattern = "[\u00fa\u00f9]",
                         replacement = "u") %>%
    stringr::str_replace(pattern = "\u00ea",
                         replacement = "e") %>%
    stringr::str_replace(pattern = "\u00f4",
                         replacement = "o") %>%
    stringr::str_replace(pattern = "\u00e2",
                         replacement = "a") %>%
    stringr::str_replace(pattern = "\u00f4",
                         replacement = "o") %>%
    stringr::str_replace(pattern = "\u00ea",
                         replacement = "e")


  # Keep names for order:
  names(diacritics) = names(word[which_diacritics])

  # Remove words with diacritics from initial vector:
  word = word[!which_diacritics]

  # Monosyllabic word:
  which_monos = stringr::str_detect(string = word, pattern = "\\.", negate = T)
  monos = stringr::str_replace_all(string = word[stringr::str_detect(word, pattern = "\\.", negate = T)],
                                   pattern = "^(.*)$",
                                   replacement = "\u02c8\\1")

  # Keep names for order:
  names(monos) = names(word[which_monos])

  # Remove monos from initial vector:
  word = word[!which_monos]

  # Word with mid-low V:
  which_mid_lows = stringr::str_detect(string = word, pattern = "[\u0254\u025b]")
  mid_lows = stringr::str_replace_all(string = word[stringr::str_detect(word, pattern = "[\u0254\u025b]")],
                                      pattern = "([:alpha:]*[\u0254\u025b])",
                                      replacement = "\u02c8\\1")

  # Keep names for order:
  names(mid_lows) = names(word[which_mid_lows])

  # Now remove mid-lows from initial vector:
  word = word[!which_mid_lows]

  # Word with final stress:
  which_heavy_finals = stringr::str_detect(string = word, pattern = "[pbtdkgszfv\u0283\u0292\u028e\u0272mnl\u027ewjiu\u00e3\u00f5ww\u0303]$")
  heavy_finals = stringr::str_replace_all(string = word[stringr::str_detect(word, pattern = "[pbtdkgszfv\u0283\u0292\u028e\u0272mnl\u027ewjiu\u00e3\u00f5ww\u0303]$")],
                                          pattern = "([:alpha:]+[pbtdkgszfv\u0283\u0292\u028e\u0272mnl\u027ewjiu\u00e3\u00f5w\u0303])$",
                                          replacement = "\u02c8\\1")

  # Keep names for order:
  names(heavy_finals) = names(word[which_heavy_finals])

  # Remove them:
  word = word[!which_heavy_finals]

  # Else, penult stress:
  penults = stringr::str_replace_all(string = word,
                                     pattern = "([:alpha:]+\\.)([:alpha:]+$)",
                                     replacement = "\u02c8\\1\\2")
  # Keep names for order:
  names(penults) = names(word)


  # Gather all words:
  output = c(monos, mid_lows, heavy_finals, penults, diacritics)
  # names(output) = c(names(monos), names(mid_lows), names(heavy_finals), names(penults), names(diacritics))

  # Revert to original order:
  output = output[order(names(output))]

  # Fix vowel height in Vl] sequences:
  output = output %>%
    stringr::str_replace(pattern = "(\u02c8\\w*)ol$",
                         replacement = "\\1\u0254l") %>%
    stringr::str_replace(pattern = "(\u02c8\\w*)el$",
                         replacement = "\\1\u025bl")

  # Change stress to penult if word ends in am:
  output = output %>%
    stringr::str_replace(pattern = "(\\w+\\.)\u02c8(\\w*am$)",
                         replacement = "\u02c8\\1\\2")

  output = output %>%
    stringr::str_replace(pattern = "am$",
                         replacement = "\u00e3w\u0303")


  return(output)

}
