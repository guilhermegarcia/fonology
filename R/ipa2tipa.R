#' \code{TIPA} translator
#'
#' Translates a phonemically transcribed sequence into \code{TIPA} commands for LaTeX.
#' The function is expected to be used in conjunction with \code{cleanText()} and \code{ipa()},
#' which provide the appropriate input for it. Note: the function will return
#' a single string, so if a vector with multiple words is provided, it will concatenate
#' all the words (keeping spaces between them) into a single output to keep the tex output
#' parsimonous and avoid multiple uses of the \code{\\textipa} function. Therefore,
#' this function isn't meant to be used to add new columns to your data frame or tibble.
#' @param string The phonemically transcribed sequence from a function such as \code{ipa()}
#' @param pre Prefix for transcription. Defaults to "/ "
#' @param post Suffix for transcription. Defaults to " /"
#' @return The tex code using \code{TIPA}
#' @examples
#' ipa2tipa(string = "bo.ni.to")
#' @export

ipa2tipa <- function(string, pre = "/ ", post = " /") {
  ipa <- string
  ipa <- stringr::str_replace_all(string, pattern = "$", replacement = "#")
  ipa <- stringr::str_split(ipa, "") |> unlist()

  # Fix affricates:
  for (i in 1:(length(ipa) - 1)) {
    if (ipa[i] == "t\u0361" & ipa[i + 1] == "\u0283") {
      ipa[i] <- "t\u0361\u0283"
      ipa[i + 1] <- ""
    } else if (ipa[i] == "d\u0361" & ipa[i + 1] == "\u0292") {
      ipa[i] <- "d\u0361\u0292"
      ipa[i + 1] <- ""
    }
  }

  ipa <- ipa[ipa != ""]

  pre <- stringr::str_c("\\textipa{ ", pre)
  post <- stringr::str_c(post, " }")

  ipa_dict <- list(
    "#" = " ",
    "i" = "i",
    "\u0250" = "5",
    "x" = "x",
    "\u026a" = "I",
    "e" = "e",
    "\u025b" = "E",
    "\u00e6" = "{\ae}",
    "a" = "a",
    "\u0251" = "A",
    "\u0254" = "O",
    "o" = "o",
    "\u028a" = "U",
    "u" = "u",
    "\u028c" = "2",
    "\u0259" = "@",
    "\u00e3" = "\\~{a}",
    "w\u0303" = "\\~{w}",
    "j\u0303" = "\\~{j}",
    "e\u0303" = "\\~{e}",
    "i\u0303" = "\\~{i}",
    "\u00f5" = "\\~{o}",
    "u\u0303" = "\\~{u}",
    "\u00f8" = "\\o",
    "\u025b\u0303" = "@",
    "\u0153\u0303" = "\\~{\\oe}",
    "\u0254\u0303" = "\\~{O}",
    "\u0251\u0303" = "\\~{A}",
    "\u0153" = "{\\oe}",
    "\u0265" = "4",
    "\u0281" = "K",
    "p" = "p",
    "b" = "b",
    "t" = "t",
    "d" = "d",
    "k" = "k",
    "g" = "g",
    "t\u0361\u0283" = "\\t{tS}",
    "d\u0361\u0292" = "\\t{dZ}",
    "f" = "f",
    "v" = "v",
    "\u03b8" = "T",
    "\u00f0" = "D",
    "s" = "s",
    "z" = "z",
    "\u0283" = "S",
    "\u0292" = "Z",
    "h" = "h",
    "m" = "m",
    "n" = "n",
    "\u014b" = "N",
    "l" = "l",
    "r" = "r",
    "\u029d" = "J",
    "j" = "j",
    "w" = "w",
    "\u0279" = "{\\*r}",
    "\u028e" = "L",
    "\u026a\u0325" = "{\\r*{I}}",
    "\u027e" = "R",
    "\u0272" = "{\\textltailn}",
    "." = ".",
    "\u02c8" = '\"',
    "\u02cc" = '{\"\"}'
  )

  output <- c(pre)

  for (i in 1:length(ipa)) {
    output[length(output) + 1] <- ipa_dict[[ipa[i]]]
  }

  output <- output[-length(output)]

  output <- stringr::str_c(output, collapse = "") |>
    stringr::str_c(post, collapse = "")

  message("Done! Here\'s your tex code using TIPA:")
  return(cat(output))
}
