#' IPA transcriber for French
#'
#' Given a string, the function returns its IPA transcription with stress and syllabification.
#' @param word A possible string in French in its orthographic form
#' @return The phonemic transcription for the string in question
#' @noRd

.fr_cache <- new.env(parent = emptyenv())

.get_fr_default_lex <- function() {
  if (!exists("default_lex", envir = .fr_cache, inherits = FALSE)) {
    default_lex <- stats::setNames(
      as.character(fr_lex$ipa),
      fr_lex$word
    )

    assign("default_lex", default_lex, envir = .fr_cache)
  }

  get("default_lex", envir = .fr_cache, inherits = FALSE)
}

ipa_fr <- function(word = "comportamento") {
  fr_ipa_lex <- .get_user_lex("fr_ipa_lex")

  wd <- stringr::str_to_lower(word) |>
    stringr::str_remove_all("[:punct:]")

  if (any(stringr::str_detect(wd, pattern = "\\d"))) {
    message("Input contains a number and will be ignored.")
  }

  wd[stringr::str_detect(wd, pattern = "\\d")] <- NA

  if (any(stringr::str_detect(stats::na.omit(wd), pattern = "-"))) {
    message("Input must be monomorphemic. Stress assignment may be incorrect.")
  }

  wd_plain <- wd
  ipa_override <- !is.na(wd) & wd %in% names(fr_ipa_lex)
  default_lex <- .get_fr_default_lex()
  matches <- !is.na(wd) & wd %in% names(default_lex)

  out <- rep(NA_character_, length(wd))

  if (any(matches)) {
    out[matches] <- unname(default_lex[wd[matches]])
  }

  unmatched <- !is.na(wd) & is.na(out)
  if (any(unmatched)) {
    out[unmatched] <- wd[unmatched] |>
      transcribe_fr() |>
      syllabify_fr()
  }

  if (any(ipa_override)) {
    out[ipa_override] <- unname(fr_ipa_lex[wd_plain[ipa_override]])
  }

  return(out)
}
