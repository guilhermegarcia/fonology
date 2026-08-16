#' Promote local lexicon entries into the shipped lexicon (maintainer only)
#'
#' Moves the corrections currently in the local lexicon layer into the
#' package's shipped lexicon layer, so that they are distributed with the
#' package instead of living only on the current machine.
#'
#' This is the maintainer half of the correction cycle:
#'
#' \enumerate{
#'   \item a wrong transcription is fixed locally with \code{add_lex_pt()} and
#'     friends, which write to the user's own data directory;
#'   \item \code{promote_lex()} folds those entries into the shipped data files
#'     in \code{data/}, which are then committed and released;
#'   \item the local layer is cleared (\code{reset = TRUE}, the default), since
#'     the corrections now ship with the package.
#' }
#'
#' Because the entries are copied into \code{data/} before the local layer is
#' cleared, nothing is lost. Removals are carried over too: a word removed with
#' \code{remove_lex_*()} is deleted from the shipped lexicon.
#'
#' The function only works from a source checkout (loaded with
#' \code{devtools::load_all()}), since an installed package has no writable
#' \code{data/} directory to promote into. After promoting, run
#' \code{devtools::document()} if the datasets are new, and commit the modified
#' \code{data/*.rda} files.
#'
#' @param lg Language(s) to promote: any of \code{"pt"}, \code{"sp"},
#'   \code{"it"}, \code{"fr"}, \code{"en"}. Defaults to \code{NULL}, meaning
#'   all of them.
#' @param reset Logical. If \code{TRUE} (default), the local layer is emptied
#'   once its entries have been promoted.
#' @return Invisibly returns a named list with, for each promoted lexicon, the
#'   number of entries added, updated, and removed.
#' @seealso \code{\link{add_lex_pt}}, \code{\link{export_lex}}
#' @examples
#' \dontrun{
#' add_lex_pt("shampoo", ipa = "\u0283am.\u02c8pu")
#' promote_lex("pt")
#' }
#' @export

promote_lex <- function(lg = NULL, reset = TRUE) {

  if (!.is_source_checkout()) {
    cli::cli_abort(c(
      "{.fn promote_lex} can only be used from a source checkout.",
      "x" = "{.pkg Fonology} is installed, so its {.file data/} directory is not editable.",
      "i" = "Clone the package and load it with {.code devtools::load_all()}."
    ))
  }

  if (is.null(lg)) lg <- names(.lex_names_by_lg)

  lg <- stringr::str_to_lower(lg)
  unknown <- setdiff(lg, names(.lex_names_by_lg))

  if (length(unknown) > 0) {
    cli::cli_abort(c(
      "Unknown language{?s} in {.arg lg}: {.val {unknown}}.",
      "i" = "Available: {.val {names(.lex_names_by_lg)}}."
    ))
  }

  names_to_promote <- unlist(.lex_names_by_lg[lg], use.names = FALSE)
  report <- list()

  for (name in names_to_promote) {
    usr <- .get_user_lex(name)
    fix <- .get_fix_lex(name)

    if (length(usr) == 0) {
      report[[name]] <- c(added = 0L, updated = 0L, removed = 0L)
      next
    }

    # NA entries are tombstones: the user removed a shipped correction.
    tombstoned <- names(usr)[is.na(usr)]
    additions <- usr[!is.na(usr)]

    added <- sum(!names(additions) %in% names(fix))
    updated <- sum(
      names(additions) %in% names(fix) &
        additions != fix[match(names(additions), names(fix))]
    )
    removed <- sum(tombstoned %in% names(fix))

    fix <- fix[!names(fix) %in% c(names(additions), tombstoned)]
    fix <- c(fix, additions)
    fix <- fix[order(names(fix))]

    .set_fix_lex(name, fix)
    .save_fix_lex(name)

    if (reset) {
      empty <- stats::setNames(character(0), character(0))
      .set_user_lex(name, empty)
      .save_user_lex(name)
    }

    report[[name]] <- c(added = added, updated = updated, removed = removed)
  }

  total <- colSums(do.call(rbind, report))

  cli::cli_alert_success(
    "Promoted {total[['added']]} new and {total[['updated']]} updated entr{?y/ies} \\
     into the shipped lexicon{?s} ({total[['removed']]} removed)."
  )
  cli::cli_alert_info("Commit the modified {.file data/*_fix.rda} files to ship them.")

  if (reset) {
    cli::cli_alert_info("Local lexicon layer cleared.")
  }

  invisible(report)
}
