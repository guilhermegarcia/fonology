#' Text cleaner
#'
#' Given a string or a vector of strings, the function tokenizes on whitespace
#' and then strips punctuation, digits, stress marks and case, returning a
#' vector of cleaned tokens.
#'
#' The return value is a vector of tokens, so it is generally \emph{not} the
#' same length as the input: a hyphenated form may yield two tokens and an
#' empty or punctuation-only element yields none. Do not call this function
#' inside \code{dplyr::mutate()} on a word-list column, where a changed length
#' is silently recycled against the other columns.
#'
#' @section Hyphens:
#' Hyphenated words in Portuguese fall into classes that call for different
#' treatment, and the function cannot reliably tell a derivational prefix
#' (\code{anti-aborto}) from a lexicalised compound (\code{guarda-chuva}), so
#' the choice is the caller's:
#'
#' \itemize{
#'   \item \code{"split"} (default) treats a hyphen as a token boundary, so
#'     \code{"guarda-chuva"} yields two tokens. This keeps the output inside
#'     the lexicon: \code{guarda} and \code{chuva} both have dictionary
#'     transcriptions.
#'   \item \code{"join"} removes the hyphen and returns one token,
#'     \code{"guardachuva"}. Useful when the prosodic word is the unit of
#'     interest, but be aware that joined forms are usually out of vocabulary,
#'     so \code{ipa()} falls back to its heuristic and marks the result.
#'   \item \code{"keep"} leaves the hyphen in place, so the caller can decide
#'     downstream.
#' }
#'
#' @param text A string, or a vector of strings already split into words
#' @param hyphen How to treat hyphens and dashes: \code{"split"} (default),
#'   \code{"join"} or \code{"keep"}. See the Hyphens section.
#' @param clitics If \code{TRUE}, remove a hyphenated Portuguese enclitic
#'   before hyphens are handled, so \code{"diz-me"} yields \code{"diz"}
#'   instead of two tokens. Portuguese-specific, and \code{FALSE} by default:
#'   dropping the clitic removes a syllable from the prosodic word, which is
#'   not always what you want.
#' @return A character vector of cleaned tokens, with no \code{NA} and no
#'   empty strings
#' @examples
#' cleanText(text = "Este \u00e9 um texto em portugu\u00eas? This is a text in English!")
#' cleanText(text = "guarda-chuva")
#' cleanText(text = "guarda-chuva", hyphen = "join")
#' cleanText(text = "diz-me", clitics = TRUE)
#' @export

cleanText <- function(text = "", hyphen = c("split", "join", "keep"),
                      clitics = FALSE) {
  hyphen <- match.arg(hyphen)

  DASHES <- "[-\u2010\u2011\u2012\u2013\u2014\u2015]"

  # Tokenize on whitespace first. Clitic removal, if requested, needs one word
  # at a time, because its pattern is anchored to the end of the string.
  tokens <- stringr::str_split(text, pattern = "\\s+") |> unlist()

  if (isTRUE(clitics)) {
    tokens <- strip_clitic_pt(tokens)
  }

  # Hyphens are handled before punctuation stripping, which would otherwise
  # delete them and silently turn every hyphenated form into "join".
  tokens <- switch(hyphen,
    split = stringr::str_split(tokens, pattern = DASHES) |> unlist(),
    join  = stringr::str_remove_all(tokens, pattern = DASHES),
    keep  = stringr::str_replace_all(tokens, DASHES, "FONOLOGYHYPHEN")
  )

  # Protect lookup/fallback markers before stripping punctuation.
  tokens <- stringr::str_replace_all(tokens, "\\*", "FONOLOGYSTAR")

  # Stress marks, digits, degree sign:
  tokens <- tokens |>
    stringr::str_remove_all(pattern = "[\u02c8\u02cc]") |>
    stringr::str_remove_all(pattern = "\\d+") |>
    stringr::str_remove_all(pattern = "\u00b0")

  # Punctuation:
  output <- stringr::str_remove_all(tokens, "[:punct:]")

  # Restore markers.
  output <- stringr::str_replace_all(output, "FONOLOGYSTAR", "*")
  output <- stringr::str_replace_all(output, "FONOLOGYHYPHEN", "-")

  # Case:
  output <- stringr::str_to_lower(output)

  # A single policy for degenerate tokens, applied at every input length: an
  # element that reduces to nothing is dropped rather than kept as NA. The
  # previous version dropped them for length-1 input and returned NA for
  # longer input, which meant nGramTbl() could receive the literal string "NA".
  output <- output[!is.na(output)]
  output <- output[output != ""]

  output
}
