#' Syllabifier for Portuguese
#'
#' Returns syllabification for a given string
#' @param word The string of interest using IPA phonemic transcription
#' @return The syllabification for the string in question
#' @examples
#' syllabify_pt(word = "komputadoɾ");
#' @importFrom magrittr %>%
#' @export

syllabify_pt = function(word = ""){

  # Start with CV:
  word = stringr::str_replace_all(string = word,
                      pattern = "([aeiou\u025b\u0254])",
                      replacement = "\\1.")

  # Fix diphthongs:
  word = stringr::str_replace_all(string = word,
                      pattern = "([aeiou\u025b\u0254])\\.([wj])",
                      replacement = "\\1\\2.")

  # Fix onset clusteres:
  word = stringr::str_replace_all(string = word,
                      pattern = "\\.([lmn\u027eskgpb])([pbtdkgsxzfv\u0283\u0292\u028e\u0272mn])",
                      replacement = "\\1.\\2")


  # Remove empty final syllables:
  word = stringr::str_remove_all(string = word,
                     pattern = "\\.$")

  # Remove C-syllables word finally:
  word = stringr::str_replace_all(string = word,
                      pattern = "\\.([pbtdkgszfv\u0283\u0292\u028elmn\u027es])$",
                      replacement = "\\1")

  word = stringr::str_replace_all(string = word,
                         pattern = "\\.([sznm])([lr])",
                         replacement = "\\1.\\2")

  # Remove h:
  word = stringr::str_remove_all(string = word,
                        pattern = "h")

  # Overly complex onset clusters:
  word = stringr::str_replace_all(string = word,
                         pattern = "\\.s([tdpbkg][\u027el])",
                         replacement = "s.\\1")

  return(word)

}
