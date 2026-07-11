#' IPA transcriber for Spanish
#'
#' Given a string, the function returns its IPA transcription with stress and
#' syllabification. Wiktionary-backed and user-override forms are returned
#' directly; regex-derived out-of-vocabulary forms are marked with \code{"*"}.
#' @param word A possible string in Spanish in its orthographic form
#' @return The phonemic transcription for the string in question
#' @noRd

.sp_cache <- new.env(parent = emptyenv())

.get_sp_default_lex <- function() {
  if (!exists("default_lex", envir = .sp_cache, inherits = FALSE)) {
    default_lex <- stats::setNames(
      as.character(sp_lex$ipa),
      sp_lex$word
    )

    assign("default_lex", default_lex, envir = .sp_cache)
  }

  get("default_lex", envir = .sp_cache, inherits = FALSE)
}

ipa_sp <- function(word = "comportamento") {
  sp_ipa_lex <- .get_user_lex("sp_ipa_lex")
  sp_lex_user <- .get_user_lex("sp_lex_user")

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
  ipa_override <- !is.na(wd) & wd %in% names(sp_ipa_lex)

  # User diacritized-form entries take priority over the corpus lexicon
  user_matches <- !is.na(wd) & wd %in% names(sp_lex_user)

  default_lex <- .get_sp_default_lex()
  lex_matches <- !is.na(wd) & !user_matches & wd %in% names(default_lex)

  out <- rep(NA_character_, length(wd))

  if (any(lex_matches)) {
    out[lex_matches] <- unname(default_lex[wd[lex_matches]])
  }

  if (any(user_matches)) {
    wd[user_matches] <- sp_lex_user[wd[user_matches]]
  }

  pipeline <- !is.na(wd) & !lex_matches
  if (any(pipeline)) {
    out[pipeline] <- wd[pipeline] |>
      transcribe_sp() |>
      syllabify_sp() |>
      stress_sp()
  }

  # Mark regex-derived (out-of-vocabulary) forms
  fallback <- pipeline & !user_matches
  if (any(fallback)) {
    out[fallback] <- stringr::str_c(out[fallback], "*")
  }

  # Apply IPA overrides last (highest priority; bypass pipeline result)
  if (any(ipa_override)) {
    out[ipa_override] <- unname(sp_ipa_lex[wd_plain[ipa_override]])
  }

  return(out)
}
