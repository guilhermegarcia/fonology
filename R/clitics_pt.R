#' Clitic removal for Portuguese
#'
#' Given a string, the function removes its hyphenated clitics.
#' @param word A possible string in Portuguese in its orthographic form
#' @return A word without its hyphenated clitics
#' @examples
#' strip_clitic_pt(word = "dá-lhe");
#' @importFrom magrittr %>%
#' @export

strip_clitic_pt = function(word = ""){

  parts = stringr::str_split(string = word,
                    pattern = "-") %>%
    unlist()

  enclitics = c("o", "a", "os", "as",
                "lo", "la", "los", "las",
                "te", "lhe", "me", "ão")


  enclitics = stringr::str_c("-", enclitics) %>%
    stringr::str_c(collapse = "|")


  proclitics = c("pré", "mini", "anti", "ante",
                 "super", "hiper", "pós", "ultra",
                 "mega", "master", "sub", "ex",
                 "pró", "recém", "além", "aquém",
                 "bem", "sem", "vice", "grão",
                 "grã", "soto")

  proclitics = stringr::str_c(proclitics, "-") %>%
    stringr::str_c(collapse = "|")

  clitics = stringr::str_c(enclitics, "|", proclitics)

  mono = stringr::str_remove_all(string = word,
                        pattern = clitics)

  return(mono)
}

