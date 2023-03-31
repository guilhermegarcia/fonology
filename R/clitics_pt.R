#' Clitic removal for Portuguese
#'
#' Given a string, the function removes its hyphenated clitics
#' @param word A possible string in Portuguese in its orthographic form
#' @noRd
#' @return A word without its hyphenated clitics

strip_clitic_pt = function(word = ""){

  parts = stringr::str_split(string = word,
                    pattern = "-") |>
    unlist()

  enclitics = c("o", "a", "os", "as",
                "lo", "la", "los", "las",
                "te", "lhe", "me", "\u00e3o")


  enclitics = stringr::str_c("-", enclitics) |>
    stringr::str_c(collapse = "|")


  proclitics = c("pr\u00e9", "mini", "anti", "ante",
                 "super", "hiper", "p\u00f3s", "ultra",
                 "mega", "master", "sub", "ex",
                 "pr\u00f3", "rec\u00e9m", "al\u00e9m", "aqu\u00e9m",
                 "bem", "sem", "vice", "gr\u00e3o",
                 "gr\u00e3", "soto")

  proclitics = stringr::str_c(proclitics, "-") |>
    stringr::str_c(collapse = "|")

  clitics = stringr::str_c(enclitics, "|", proclitics)

  mono = stringr::str_remove_all(string = word,
                        pattern = clitics)

  return(mono)
}

