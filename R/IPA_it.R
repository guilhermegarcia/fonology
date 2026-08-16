#' IPA transcriber for Italian
#'
#' Given a string, the function returns its IPA transcription with stress and
#' syllabification. Wiktionary-backed and user-override forms are returned
#' directly; regex-derived out-of-vocabulary forms are marked with \code{"*"}.
#' @param word A possible string in Italian in its orthographic form
#' @return The phonemic transcription for the string in question
#' @noRd

.it_cache <- new.env(parent = emptyenv())

.get_it_default_lex <- function() {
  if (!exists("default_lex", envir = .it_cache, inherits = FALSE)) {
    default_lex <- stats::setNames(
      as.character(it_lex$ipa),
      it_lex$word
    )

    assign("default_lex", default_lex, envir = .it_cache)
  }

  get("default_lex", envir = .it_cache, inherits = FALSE)
}

ipa_it <- function(word = "italiano") {
  it_ipa_lex <- .lex("it_ipa_lex")
  it_lex_user <- .lex("it_lex_user")

  wd <- stringr::str_to_lower(word) |>
    stringr::str_remove_all("[:punct:]")

  # Set digit-containing words to NA (vectorized; does not affect other entries)
  wd[stringr::str_detect(wd, "\\d")] <- NA

  if (any(stringr::str_detect(stats::na.omit(wd), "-"))) {
    cli::cli_alert_warning("Input must be monomorphemic; stress assignment may be incorrect.")
  }

  # Save plain keys before any substitution
  wd_plain <- wd
  ipa_override <- !is.na(wd) & wd %in% names(it_ipa_lex)

  # User diacritized-form entries take priority over the corpus lexicon: the
  # diacritized form is injected so stress_it()'s diacritic rule places
  # stress and vowel quality correctly.
  user_matches <- !is.na(wd) & wd %in% names(it_lex_user)

  default_lex <- .get_it_default_lex()
  lex_matches <- !is.na(wd) & !user_matches & wd %in% names(default_lex)

  out <- rep(NA_character_, length(wd))

  if (any(lex_matches)) {
    out[lex_matches] <- unname(default_lex[wd[lex_matches]])
  }

  if (any(user_matches)) {
    wd[user_matches] <- it_lex_user[wd[user_matches]]
  }

  # Run pipeline on the remaining entries only (stress_it uses names-based
  # tracking that does not tolerate NA elements in the input vector)
  pipeline <- !is.na(wd) & !lex_matches
  if (any(pipeline)) {
    out[pipeline] <- wd[pipeline] |>
      transcribe_it() |>
      syllabify_it() |>
      stress_it()
  }

  # Mark regex-derived (out-of-vocabulary) forms
  fallback <- pipeline & !user_matches
  if (any(fallback)) {
    out[fallback] <- stringr::str_c(out[fallback], "*")
  }

  # Apply IPA overrides last (highest priority; bypass pipeline result)
  if (any(ipa_override)) {
    out[ipa_override] <- unname(it_ipa_lex[wd_plain[ipa_override]])
  }

  return(out)
}
