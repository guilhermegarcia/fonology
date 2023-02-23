#' IPA tester for ipa_pt function in Fonology package
#'
#' Returns a list of words. No arguments needed.
#' @return A list of words broadly and narrowly transcribed using ipa_pt
#' @examples
#' ipa_pt_test();
#' @export

ipa_pt_test = function(){
  if(!require("pacman", quietly = T)){install.packages("pacman")}
  pacman::p_load(tidyverse)

  require(tidyverse, quietly = T)

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
                "dane",
                "mylena",
                "parangaricutirrimirruaro")


  for(i in 1:length(testWords)){

    message(str_c(testWords[i], ":"))

    ipa_pt(testWords[i]) %>%
      print()

    ipa_pt(testWords[i], narrow = T) %>%
      print()

    message("========================")
  }


}
