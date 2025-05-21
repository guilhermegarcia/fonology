#' Syllable shape for transcribed strings
#'
#' Given a phonemically transcribed string, the function returns its syllable shape using C, V and G to represent consonants, vowels and glides, respectively.
#'
#' @param word A possible string in its phonemic form, i.e., the output of \code{ipa()}.
#' @param stress Whether the stress diacritic from \code{ipa()} should be kept in the output (default is \code{FALSE})
#' @return The syllable shape of the string
#' @examples
#' cv(word = "pa.ˈklo.do")
#' @importFrom stringi stri_trans_nfd stri_trans_nfc
#' @export

cv <- function(word = c(""), stress = FALSE) {
  output <- word |>
    # NOTE: First, remove nasal diacritic
    stringi::stri_trans_nfd() |>
    stringr::str_remove_all("\u0303") |>
    stringi::stri_trans_nfc() |>
    stringr::str_replace_all(
      pattern = "[jww\u0303\u0265]", # NOTE: Glides
      replacement = "G"
    ) |>
    stringr::str_replace_all(
      pattern = "[aeiou\u0251\u00e6\u0250\u0259\u025a\u0275\u0258\u025b\u025c\u025d\u025e\u026a\u0268\u0254\u0153\u0252\u0276\u00f8\u028a\u0289\u028c\u026f\u0264\u028f]", # NOTE: Vowels
      replacement = "V"
    ) |>
    stringr::str_replace_all(
      pattern = "t\u0361\u0283|d\u0361\u0292|t\u0361s|d\u0361z", # NOTE: Affricates
      replacement = "C"
    ) |>
    stringr::str_replace_all(
      pattern = "[\u03b2\u0253\u0299\u00e7\u0255\u00f0\u0256\u0257\u0260\u0262\u029b\u0261\u0127\u0266\u0265\u0267\u029c\u029d\u025f\u0284\u026b\u026d\u026c\u029f\u026e\u0271\u014b\u0272\u0273\u0274\u0278\u027e\u0279\u0281\u0280\u027b\u027d\u027a\u0283\u0282\u03b8\u0288\u028b\u2c71\u028d\u0270\u03c7\u0263\u028e\u0292\u0290\u0291\u0294\u0295\u02a1\u02a2bcdfghklmnpqrstvxz]", # NOTE: Consonants
      replacement = "C"
    )

  if (stress) {
    return(output)
  }

  output <- output |>
    stringr::str_remove_all(pattern = "\u02c8")
  return(output)
}
