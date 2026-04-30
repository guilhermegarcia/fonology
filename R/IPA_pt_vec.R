#' Vectorized IPA transcriber for Portuguese
#'
#' Given a string, the function returns its IPA transcription with stress and syllabification.
#' @param word A possible string in Portuguese in its orthographic form
#' @param narrow Boolean. Whether a narrow transcription is desired (defaults to \code{FALSE})
#' @return The phonemic transcription for the string in question
#' @noRd

ipa_pt_vec <- function(word = c("palavra"), narrow = FALSE) {
  pt_ipa_lex <- .get_user_lex("pt_ipa_lex")
  pt_lex_user <- .get_user_lex("pt_lex_user")

  wd <- stringr::str_to_lower(word) |>
    stringr::str_remove_all(pattern = "[:punct:]") |>
    strip_clitic_pt()

  wd[stringr::str_detect(wd, "\\d")] <- NA

  # Save plain keys before any substitution so we can apply IPA overrides later
  wd_plain <- wd
  ipa_override <- !is.na(wd) & wd %in% names(pt_ipa_lex)

  user_matches <- !is.na(wd) & wd %in% names(pt_lex_user)
  if (any(user_matches)) wd[user_matches] <- pt_lex_user[wd[user_matches]]

  lex_matches <- !is.na(wd) & !user_matches & wd %in% pt_lex$word
  if (any(lex_matches)) {
    lex_idx <- match(wd[lex_matches], pt_lex$word)
    wd[lex_matches] <- pt_lex$pro[lex_idx] |>
      stringr::str_replace(pattern = "\'", replacement = "\u02c8") |>
      stringr::str_replace(pattern = "\u027e", replacement = "r") |>
      stringr::str_replace(
        pattern = "\u028ee\u027e$",
        replacement = "\u028e\u025br"
      )
  }

  fallback <- !is.na(wd) & !lex_matches
  if (any(fallback)) {
    wd[fallback] <- wd[fallback] |>
      transcribe_pt_vec() |>
      syllabify_pt_vec() |>
      stress_pt_vec() |>
      stringr::str_remove_all(pattern = "\\.$")
  }

  # Check for narrow transcription:
  if (narrow == T) {
    not_na <- !is.na(wd)
    wd[not_na] <- wd[not_na] |>
      narrow_pt_vec()
  }

  # Apply IPA overrides last (highest priority; bypass result of pipeline)
  if (any(ipa_override)) {
    wd[ipa_override] <- unname(pt_ipa_lex[wd_plain[ipa_override]])
  }

  return(wd)
}
