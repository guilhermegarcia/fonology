#' Phoneme generator from distinctive features
#'
#' Returns a set of phonemes given different distinctive feature values.
#' @param ft The abbreviated features of interest: \code{syl}, \code{son},
#' \code{cons}, \code{cont}, \code{DR}, \code{lat}, \code{nas}, \code{strid},
#' \code{vce}, \code{sg}, \code{cg}, \code{ant}, \code{cor}, \code{distr},
#' \code{lab}, \code{hi}, \code{lo}, \code{back}, \code{round}, \code{vel},
#' \code{tense}, \code{long}, \code{hitone}, \code{hireg}, \code{approx}.
#' Each must be immediately preceded by \code{+}, \code{-} or \code{0}, and no
#' feature may be given twice
#' @param lg The language of interest: English, French, Italian, Portuguese, Spanish.
#' Alternatively, you can also provide your own inventory as a vector. Every
#' segment must be listed in \code{allFeatures}; unknown segments raise an error
#' rather than being ignored
#' @return The phonemes given the features provided, in inventory order and in
#' the notation used by the inventory. If nothing matches, the message
#' \code{"No phonemes with the features provided given the inventory in question."}
#' is returned instead
#' @examples
#' getPhon(ft = c("+hi", "+tense"), lg = "english")
#' getPhon(ft = c("+hi", "+tense"), lg = c("i", "u", "a", "p", "t", "k", "l", "m", "n"))
#' getPhon(ft = c("-son", "+vce"), lg = "spanish")
#' @seealso \code{\link{getFeat}} for the inverse operation
#' @export

getPhon <- function(ft, lg) {
  ft <- as.character(ft)

  if (length(ft) == 0) {
    cli::cli_abort("No features provided.")
  }

  # Split at the first character rather than matching feature names with a
  # regex: an alternation is leftmost-first, so "hi" would shadow "hitone" and
  # "lo" would shadow "long".
  values <- substr(ft, 1, 1)
  features <- substr(ft, 2, nchar(ft))

  invalid <- ft[!values %in% c("+", "-", "0") | !features %in% .feature_names]

  if (length(invalid) > 0) {
    cli::cli_abort(c(
      "Incorrect feature{?s}: {.val {invalid}}.",
      "i" = "Every feature must be immediately preceded by {.code +}, {.code -}, or {.code 0}.",
      "i" = "See {.fn getPhon} for the features available."
    ))
  }

  if (anyDuplicated(features)) {
    cli::cli_abort(c(
      "Each feature may only be given once.",
      "x" = "Repeated: {.val {unique(features[duplicated(features)])}}."
    ))
  }

  targetLanguage <- .resolve_lg(lg)
  targetF <- .feature_table(targetLanguage)

  keep <- rep(TRUE, nrow(targetF))

  for (i in seq_along(features)) {
    keep <- keep & targetF[[features[i]]] == values[i]
  }

  if (!any(keep)) {
    return("No phonemes with the features provided given the inventory in question.")
  }

  targetF$ipa[keep]
}
