#' Vectorized stress assigner for Portuguese
#'
#' Given a string, the function assigns regular stress to a vector of strings
#' Stress is categorically defined, so the function is more simplistic than stress_pt().
#' and probabilistic distributions in certain cases for hypothetical words
#' @param word A vector with possible strings in Portuguese, which must be phonemically transcribed and syllabified
#' @return The stressed strings
#' @examples
#' stress_pt_simple(word = c("pa.la.do", "an.te.dom"));
#' @importFrom magrittr %>%
#' @export

stress_pt_simple = function(word = c("ka.va.lo")){

  # Monosyllabic word:
  which_monos = stringr::str_detect(string = word, pattern = "\\.", negate = T)
  monos = stringr::str_replace_all(string = word[stringr::str_detect(word, pattern = "\\.", negate = T)],
                          pattern = "^(.*)$",
                          replacement = "\u02c8\\1")
  # Remove monos from initial vector:
  word = word[!which_monos]

  # Word with mid-low V:
  which_mid_lows = stringr::str_detect(string = word, pattern = "[\u0254\u025b]")
  mid_lows = stringr::str_replace_all(string = word[stringr::str_detect(word, pattern = "[\u0254\u025b]")],
                             pattern = "([:alpha:]*[\u0254\u025b])",
                             replacement = "\u02c8\\1")

  # Now remove mid-lows from initial vector:
  word = word[!which_mid_lows]

  # Word with final stress:
  which_heavy_finals = stringr::str_detect(string = word, pattern = "[pbtdkgszfv\u0283\u0292\u028e\u0272mnl\u027ewjiu\u00e3\u00f5ww\u0303]$")
  heavy_finals = stringr::str_replace_all(string = word[stringr::str_detect(word, pattern = "[pbtdkgszfv\u0283\u0292\u028e\u0272mnl\u027ewjiu\u00e3\u00f5ww\u0303]$")],
                                 pattern = "([:alpha:]+[pbtdkgszfv\u0283\u0292\u028e\u0272mnl\u027ewjiu\u00e3\u00f5w\u0303])$",
                                 replacement = "\u02c8\\1")

  # Remove them:
  word = word[!which_heavy_finals]

  # Else, penult stress:
  penults = stringr::str_replace_all(string = word,
                            pattern = "([:alpha:]+\\.)([:alpha:]+$)",
                            replacement = "\u02c8\\1\\2")


  output = c(monos, mid_lows, heavy_finals, penults)


  return(output)

}
