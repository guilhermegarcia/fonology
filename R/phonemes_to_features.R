#' Distinctive feature generator
#'
#' Generates the minimal feature matrix that picks out a given set of phonemes,
#' and only that set, within a given inventory. If no set of features isolates
#' the phonemes provided, they do not form a natural class in that language.
#' @param ph The phonemes of interest
#' @param lg The language of interest: English, French, Italian, Portuguese, Spanish.
#' Alternatively, you can also provide your own inventory as a vector. Every
#' segment must be listed in \code{allFeatures}; unknown segments raise an error
#' rather than being ignored
#' @return The minimal matrix of features given \code{ph} and \code{lg}, as a
#' character vector such as \code{c("-son", "-cont", "+lab")}. If the phonemes
#' do not form a natural class, the message
#' \code{"Not a natural class in this language."} is returned instead
#' @details The built-in inventories (\code{vowels_pt}, \code{consonants_pt}
#' and so on) list the phonemes a language is analysed as having, not the
#' symbols \code{\link{ipa}} prints. Portuguese, for instance, is analysed with
#' oral vowels only. To work with surface segments, pass the inventory you want
#' as \code{lg} instead of a language name.
#' @examples
#' getFeat(ph = c("i", "u"), lg = "english")
#' getFeat(ph = c("i", "u"), lg = c("i", "u", "a", "p", "t", "k", "l", "m", "n"))
#' getFeat(ph = c("p", "b"), lg = "portuguese")
#' @seealso \code{\link{getPhon}} for the inverse operation
#' @export

getFeat <- function(ph, lg) {
  targetLanguage <- .resolve_lg(lg)

  # Feature table for the inventory. Column order matters: it decides which
  # matrix wins when several are equally small, so the historical order
  # (ipa, syl, cons, son, then cont onwards) is preserved.
  targetF <- .feature_table(targetLanguage) |>
    dplyr::select("ipa", "syl", "cons", "son", "cont":"approx")

  ph <- as.character(ph)

  if (length(ph) == 0) {
    cli::cli_abort("No phonemes provided.")
  }

  phKey <- .norm_ipa(ph)
  invKey <- .norm_ipa(targetF$ipa)
  unknown <- ph[!phKey %in% invKey]

  if (length(unknown) > 0) {
    cli::cli_abort(c(
      "Input doesn't match phonemic inventory in language.",
      "x" = "Not in the inventory: {.val {unique(unknown)}}."
    ))
  }

  isChosen <- invKey %in% unique(phKey)
  chosenPhF <- targetF[isChosen, , drop = FALSE]

  # Features that are constant across the whole inventory cannot distinguish
  # anything, so they can never belong to a minimal matrix.
  candidates <- setdiff(names(targetF), "ipa")
  candidates <- candidates[vapply(
    targetF[candidates],
    function(column) dplyr::n_distinct(column) > 1,
    logical(1)
  )]

  if (nrow(chosenPhF) == nrow(targetF)) {
    return("No distinguishing features: this is the entire inventory.")
  }

  # A minimal matrix can only be built from features the chosen phonemes agree
  # on, so start from their intersection.
  shared <- candidates[vapply(
    chosenPhF[candidates],
    function(column) dplyr::n_distinct(column) == 1,
    logical(1)
  )]

  if (length(shared) == 0) {
    return("Not a natural class in this language.")
  }

  values <- vapply(chosenPhF[shared], function(column) column[1], character(1))

  # How many inventory segments satisfy a given subset of the shared features?
  # Every chosen phoneme satisfies all of them by construction, so a count
  # equal to the size of the chosen set means the subset picks out exactly it.
  nMatched <- function(features) {
    keep <- rep(TRUE, nrow(targetF))

    for (feature in features) {
      keep <- keep & targetF[[feature]] == values[[feature]]
    }

    sum(keep)
  }

  if (nMatched(shared) != nrow(chosenPhF)) {
    return("Not a natural class in this language.")
  }

  # Search by increasing size, so the first size that works is minimal. Within a
  # size, prefer the matrix with the fewest "0" values: a feature that is simply
  # not applicable to the segments can distinguish them, but it says nothing
  # about them, so [+nas, +lo] is a better description of a nasal low vowel than
  # [0DR, +nas]. Remaining ties are broken by feature order, as before.
  for (size in seq_along(shared)) {
    subsets <- utils::combn(length(shared), size)
    best <- NULL
    bestZeros <- Inf

    for (j in seq_len(ncol(subsets))) {
      features <- shared[subsets[, j]]

      if (nMatched(features) == nrow(chosenPhF)) {
        zeros <- sum(values[features] == "0")

        if (zeros < bestZeros) {
          best <- features
          bestZeros <- zeros
        }

        if (bestZeros == 0) {
          break
        }
      }
    }

    if (!is.null(best)) {
      return(unname(stringr::str_c(values[best], best)))
    }
  }
}
