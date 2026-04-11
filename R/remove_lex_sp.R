#' Remove words from the Spanish user lexicon
#'
#' Removes one or more words from the Spanish user lexicon (both the
#' diacritized-form store and the IPA-override store). Plain orthographic
#' forms are accepted; diacritized forms are also accepted and the plain key
#' is derived automatically via diacritic stripping.
#'
#' @param words A character vector of words to remove (plain or diacritized
#'   orthographic forms).
#' @return Invisibly returns a list with the updated lexicons:
#'   \code{diacritized} (\code{sp_lex}) and \code{ipa} (\code{sp_ipa_lex}).
#' @seealso \code{\link{add_lex_sp}}, \code{\link{export_lex}}
#' @noRd

remove_lex_sp <- function(words) {

  plain <- stringi::stri_trans_nfd(words) |>
    stringr::str_remove_all("\\p{M}")

  ## ---- Diacritized-form lexicon -------------------------------------------
  lex_d <- .get_user_lex("sp_lex")
  lex_d <- lex_d[!names(lex_d) %in% plain]

  .set_user_lex("sp_lex", lex_d)
  .save_user_lex("sp_lex")

  ## ---- IPA-override lexicon -----------------------------------------------
  lex_i <- .get_user_lex("sp_ipa_lex")
  lex_i <- lex_i[!names(lex_i) %in% plain]

  .set_user_lex("sp_ipa_lex", lex_i)
  .save_user_lex("sp_ipa_lex")

  invisible(list(diacritized = lex_d, ipa = lex_i))
}
