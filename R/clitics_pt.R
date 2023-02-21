#' Clitic removal for Portuguese
#'
#' Given a string, the function removes its hyphenated clitics.
#' @param word A possible string in Portuguese in its orthographic form
#' @return A word without its hyphenated clitics
#' @examples
#' strip_clitic_pt(word = "dá-lhe");
#' @export

strip_clitic_pt = function(word = ""){

  if(!require("pacman", quietly = T)){install.packages("pacman")}
  pacman::p_load(tidyverse)


  parts = str_split(string = word,
                    pattern = "-") %>%
    unlist()

  enclitics = c("o", "a", "os", "as",
                "lo", "la", "los", "las",
                "te", "lhe", "me", "ão")


  enclitics = str_c("-", enclitics) %>%
    str_c(collapse = "|")


  proclitics = c("pré", "mini", "anti", "ante",
                 "super", "hiper", "pós", "ultra",
                 "mega", "master", "sub", "ex",
                 "pró", "recém", "além", "aquém",
                 "bem", "sem", "vice", "grão",
                 "grã", "soto")

  proclitics = str_c(proclitics, "-") %>%
    str_c(collapse = "|")

  clitics = str_c(enclitics, "|", proclitics)

  mono = str_remove_all(string = word,
                        pattern = clitics)

  return(mono)
}

