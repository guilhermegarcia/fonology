#' Add words to the Italian lexicon
#'
#' Adds one or more Italian words to the user lexicon used by
#' \code{ipa(lg = "Italian")}.
#'
#' \strong{Diacritized-form mode} (\code{ipa = NULL}, default): supply words
#' with diacritics encoding stress position and/or mid-vowel quality. The plain
#' orthographic key is derived automatically, and the diacritized form is
#' injected into the transcription pipeline. A file path (one diacritized word
#' per line) may be passed instead of a character vector.
#'
#' \strong{IPA-override mode} (\code{ipa} not \code{NULL}): supply the plain
#' orthographic form(s) in \code{words} and the corresponding IPA string(s) in
#' \code{ipa}. The IPA is stored verbatim and returned directly—the
#' transcription pipeline is bypassed entirely.
#'
#' IPA-override entries take priority over diacritized-form entries.
#'
#' Diacritic conventions for Italian (diacritized-form mode):
#' \describe{
#'   \item{\code{é}}{Stressed open-mid /\enc{ɛ}{e}/ (stress on this syllable, open e)}
#'   \item{\code{ê}}{Stressed close-mid /e/ (stress on this syllable, close e)}
#'   \item{\code{è}}{Stressed open-mid /\enc{ɛ}{e}/ on final syllable (Italian orthographic convention)}
#'   \item{\code{ó}}{Stressed open-mid /\enc{ɔ}{o}/ (stress on this syllable, open o)}
#'   \item{\code{ô}}{Stressed close-mid /o/ (stress on this syllable, close o)}
#'   \item{\code{ò}}{Stressed open-mid /\enc{ɔ}{o}/ on final syllable}
#'   \item{\code{à}, \code{ì}, \code{ù}}{Stress-only markers (no quality distinction)}
#' }
#'
#' @param words A character vector of Italian words. In diacritized-form mode,
#'   supply the diacritized orthographic forms; in IPA-override mode, supply
#'   plain orthographic forms. Alternatively (diacritized-form mode only), a
#'   path to a plain-text file with one word per line.
#' @param ipa \code{NULL} (default) or a character vector of IPA strings the
#'   same length as \code{words}.
#' @return Invisibly returns the updated lexicon named character vector.
#' @seealso \code{\link{export_lex}}
#' @examples
#' \dontrun{
#' # Fix stress / mid-vowel quality (supply the diacritized form you want):
#' add_lex_it(c("bl\u00e9rico", "tr\u00f4vico"))
#' ipa("blerico", lg = "it")
#'
#' # Fix an entirely wrong transcription (supply the target IPA directly):
#' add_lex_it("flopranto", ipa = "flo.\u02c8pran.to")
#' ipa("flopranto", lg = "it")
#'
#' # Import from a shared file (diacritized-form mode only):
#' add_lex_it("italian_entries.txt")
#' }
#' @export

add_lex_it <- function(words, ipa = NULL) {

  ## ---- IPA-override mode --------------------------------------------------
  if (!is.null(ipa)) {
    if (length(words) != length(ipa)) {
      stop("`words` and `ipa` must have the same length.")
    }

    plain <- words
    keep  <- !duplicated(plain, fromLast = TRUE)
    plain <- plain[keep]
    ipa   <- ipa[keep]

    lex <- get("it_ipa_lex", envir = as.environment("package:Fonology"))

    new_entries       <- ipa
    names(new_entries) <- plain
    lex <- c(lex[!names(lex) %in% plain], new_entries)

    it_ipa_lex <- lex
    save(it_ipa_lex,
         file = file.path(find.package("Fonology"), "data", "it_ipa_lex.rda"),
         compress = "xz")

    .lex_assign("it_ipa_lex", lex)

    return(invisible(lex))
  }

  ## ---- Diacritized-form mode (existing behaviour) -------------------------
  if (length(words) == 1 && file.exists(words)) {
    words <- readLines(words, warn = FALSE)
    words <- words[nzchar(words)]
  }

  plain <- stringi::stri_trans_nfd(words) |>
    stringr::str_remove_all("\\p{M}")

  keep <- !duplicated(plain, fromLast = TRUE)
  words <- words[keep]
  plain <- plain[keep]

  lex <- get("it_lex", envir = as.environment("package:Fonology"))

  new_entries <- words
  names(new_entries) <- plain
  lex <- c(lex[!names(lex) %in% plain], new_entries)

  it_lex <- lex
  save(it_lex, file = file.path(find.package("Fonology"), "data", "it_lex.rda"),
       compress = "xz")

  .lex_assign("it_lex", lex)

  invisible(lex)
}
