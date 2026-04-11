#' Remove words from the Portuguese user lexicon
#'
#' Removes one or more words from the Portuguese user lexicon (both the
#' diacritized-form store and the IPA-override store). Plain orthographic
#' forms are accepted; diacritized forms are also accepted and the plain key
#' is derived automatically via diacritic stripping.
#'
#' @param words A character vector of words to remove (plain or diacritized
#'   orthographic forms).
#' @return Invisibly returns a list with the updated lexicons:
#'   \code{diacritized} (\code{pt_lex_user}) and \code{ipa} (\code{pt_ipa_lex}).
#' @seealso \code{\link{add_lex_pt}}, \code{\link{export_lex}}
#' @noRd

remove_lex_pt <- function(words) {

  plain <- stringi::stri_trans_nfd(words) |>
    stringr::str_remove_all("\\p{M}")

  ## ---- Diacritized-form lexicon -------------------------------------------
  lex_d <- .get_user_lex("pt_lex_user")
  lex_d <- lex_d[!names(lex_d) %in% plain]

  .set_user_lex("pt_lex_user", lex_d)
  .save_user_lex("pt_lex_user")

  ## ---- IPA-override lexicon -----------------------------------------------
  lex_i <- .get_user_lex("pt_ipa_lex")
  lex_i <- lex_i[!names(lex_i) %in% plain]

  .set_user_lex("pt_ipa_lex", lex_i)
  .save_user_lex("pt_ipa_lex")

  invisible(list(diacritized = lex_d, ipa = lex_i))
}
