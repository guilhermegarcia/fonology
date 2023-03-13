#' IPA tester for ipa_pt function in Fonology package
#'
#' Returns a list of words. No arguments needed.
#' @return A list of words broadly and narrowly transcribed using ipa_pt
#' @examples
#' ipa_pt_test();
#' @importFrom magrittr %>%
#' @export

ipa_pt_test = function(){
  message("Broad and narrow transcriptions using ipa_pt():")


  testWords = c("strada",
                "cavalo",
                "cam\u00f3tipo",
                "paitrado",
                "m\u00eatilo",
                "frantidolanildo",
                "wagmo",
                "lispico",
                "fadist\u00e3o",
                "frinte",
                "ful",
                "catto",
                "dane",
                "mylena",
                "parangaricutirrimirruaro")


  for(i in 1:length(testWords)){

    message(stringr::str_c(testWords[i], ":"))

    ipa_pt(testWords[i]) %>%
      print()

    ipa_pt(testWords[i], narrow = T) %>%
      print()

    message("========================")
  }

  message("Vectorized version using ipa_pt_vec():")
  return(ipa_pt_vec(testWords))

}
