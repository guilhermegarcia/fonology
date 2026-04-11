#' Add words to the Spanish lexicon
#'
#' Adds one or more Spanish words to the user lexicon used by
#' \code{ipa(lg = "Spanish")}.
#'
#' \strong{Diacritized-form mode} (\code{ipa = NULL}, default): supply words
#' with stress diacritics. The plain orthographic key is derived automatically,
#' and the diacritized form is injected into the transcription pipeline. A file
#' path (one diacritized word per line) may be passed instead of a character
#' vector.
#'
#' \strong{IPA-override mode} (\code{ipa} not \code{NULL}): supply the plain
#' orthographic form(s) in \code{words} and the corresponding IPA string(s) in
#' \code{ipa}. The IPA is stored verbatim and returned directly—the
#' transcription pipeline is bypassed entirely.
#'
#' Changes made by this function are local to the current package
#' installation. When used from a source checkout loaded with
#' \code{devtools::load_all()}, the corresponding data file in the source tree
#' is updated and can be committed.
#'
#' IPA-override entries take priority over diacritized-form entries.
#'
#' @param words A character vector of Spanish words. In diacritized-form mode,
#'   supply the diacritized orthographic forms; in IPA-override mode, supply
#'   plain orthographic forms. Alternatively (diacritized-form mode only), a
#'   path to a plain-text file with one word per line.
#' @param ipa \code{NULL} (default) or a character vector of IPA strings the
#'   same length as \code{words}.
#' @return Invisibly returns the updated lexicon named character vector.
#' @seealso \code{\link{export_lex}}
#' @examples
#' \dontrun{
#' # Fix stress (supply the diacritized form you want):
#' add_lex_sp(c("fl\u00f3rico", "pl\u00e1vico"))
#' ipa("florico", lg = "sp")
#'
#' # Fix an entirely wrong transcription (supply the target IPA directly):
#' add_lex_sp("flopranto", ipa = "flo.\u02c8pran.to")
#' ipa("flopranto", lg = "sp")
#'
#' # Import from a shared file (diacritized-form mode only):
#' add_lex_sp("spanish_entries.txt")
#' }
#' @export

add_lex_sp <- function(words, ipa = NULL) {

  ## ---- IPA-override mode --------------------------------------------------
  if (!is.null(ipa)) {
    if (length(words) != length(ipa)) {
      stop("`words` and `ipa` must have the same length.")
    }

    plain <- words
    keep  <- !duplicated(plain, fromLast = TRUE)
    plain <- plain[keep]
    ipa   <- ipa[keep]

    lex <- .get_user_lex("sp_ipa_lex")

    new_entries       <- ipa
    names(new_entries) <- plain
    lex <- c(lex[!names(lex) %in% plain], new_entries)

    .set_user_lex("sp_ipa_lex", lex)
    .save_user_lex("sp_ipa_lex")

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

  lex <- .get_user_lex("sp_lex")

  new_entries <- words
  names(new_entries) <- plain
  lex <- c(lex[!names(lex) %in% plain], new_entries)

  .set_user_lex("sp_lex", lex)
  .save_user_lex("sp_lex")

  invisible(lex)
}
