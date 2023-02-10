#' IPA tester for ipa_pt function in Fonology package
#'
#' Returns a list of words. No arguments needed.
#' @return A list of words broadly and narrowly transcribed using ipa_pt
#' @examples
#' ;
#' @export

test_pt = function(){
  require(Fonology)
  require(tidyverse)

  testWords = c("strada",
                "cavalo",
                "paitrado",
                "metilo",
                "frantidolanildo",
                "wagmo",
                "lispico",
                "fadistão",
                "frinte",
                "ful",
                "catto",
                "malli",
                "mylena",
                "parangaricutirrimirruaro")


  for(i in 1:length(testWords)){
    ipa_pt(testWords[i]) %>%
      print()

    ipa_pt(testWords[i], narrow = T) %>%
      print()

    print("========================")
  }


}
