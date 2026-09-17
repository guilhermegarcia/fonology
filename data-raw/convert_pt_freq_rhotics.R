# One-off migration of pt_freq to the phonemic rhotic convention (1.7.1).
#
# There is no build script for pt_freq, so the stored object is converted rather
# than rebuilt. The script refuses to run on an object that has already been
# converted.
#
# In the stored transcriptions the tap is always r, so r -> \u027e is safe. x is
# EITHER the strong rhotic OR an orthographic x copied through by an older
# transcriber (m\u00e1ximo -> \u02c8ma.xi.mo). Orthographic x is unpredictable in
# Portuguese (\u0283, ks, s, z), so those are deliberately left untouched: only an
# x that corresponds to a strong-rhotic context in the orthography (initial r,
# rr, or r after n m s l z) is rewritten as r.
#
# Each x-producing event in the orthography is matched, left to right, with the
# x's in the transcription. A strong rhotic always yields exactly one x; an
# orthographic x yields one x only if more x's remain than strong rhotics still
# to be matched. Two cases need no matching: a transcription without x, and a
# word without an orthographic x, where every x can only be the rhotic. Note
# that the old transcriber did not always apply the strong rhotic after l
# (melro -> ma.lro); such forms are converted faithfully, with a tap, rather
# than reanalysed. Words where the counts cannot be reconciled are left as
# they are and listed.
#
# apu, pu and u are the last three syllables of ipa (verified identical on the
# shipped object) and are regenerated from the converted transcription.

pkgload::load_all(".")

stopifnot(!any(stringr::str_detect(pt_freq$ipa, "\u027e"), na.rm = TRUE))

events <- function(word) {
  w <- stringr::str_to_lower(word)
  loc <- stringr::str_locate_all(w, "rr|r|x")[[1]]
  if (nrow(loc) == 0) return(character(0))
  vapply(seq_len(nrow(loc)), function(i) {
    tok <- substr(w, loc[i, 1], loc[i, 2])
    if (tok == "rr") return("R")
    if (tok == "x") return("X")
    prev <- if (loc[i, 1] == 1) "" else substr(w, loc[i, 1] - 1, loc[i, 1] - 1)
    if (prev == "" || prev %in% c("n", "m", "s", "l", "z")) "R" else "T"
  }, character(1))
}

convert_one <- function(word, ipa) {
  if (is.na(ipa)) return(list(ipa = ipa, ok = TRUE))
  ev <- events(word)
  ev <- ev[ev %in% c("R", "X")]
  pos <- stringr::str_locate_all(ipa, "x")[[1]][, 1]
  n_x <- length(pos)
  tapped <- stringr::str_replace_all(ipa, "r", "\u027e")
  # Nothing to classify.
  if (n_x == 0) return(list(ipa = tapped, ok = TRUE))
  # No orthographic x: every x in the transcription can only be the rhotic.
  if (!grepl("x", stringr::str_to_lower(word), fixed = TRUE)) {
    return(list(ipa = stringr::str_replace_all(tapped, "x", "r"), ok = TRUE))
  }
  remaining_R <- sum(ev == "R")
  k <- 1
  rhotic <- integer(0)
  ok <- TRUE
  for (e in ev) {
    if (e == "R") {
      if (k > n_x) { ok <- FALSE; break }
      rhotic <- c(rhotic, pos[k])
      k <- k + 1
      remaining_R <- remaining_R - 1
    } else if ((n_x - k + 1) > remaining_R) {
      k <- k + 1
    }
  }
  if (ok && k != n_x + 1) ok <- FALSE
  out <- tapped
  if (ok) for (p in rhotic) stringr::str_sub(out, p, p) <- "r"
  list(ipa = out, ok = ok)
}

res <- mapply(convert_one, pt_freq$word, pt_freq$ipa, SIMPLIFY = FALSE)
new_ipa <- vapply(res, `[[`, character(1), "ipa")
unresolved <- pt_freq$word[!vapply(res, `[[`, logical(1), "ok")]
if (length(unresolved)) {
  message(length(unresolved), " word(s) left with x unconverted: ",
          paste(unresolved, collapse = ", "))
}

syl <- stringr::str_split(stringr::str_remove_all(new_ipa, "\u02c8"), stringr::fixed("."))
pick <- function(k) unname(vapply(syl, function(s) if (length(s) >= k) s[length(s) - k + 1] else NA_character_, character(1)))

pt_freq$ipa <- unname(new_ipa)
pt_freq$u <- pick(1)
pt_freq$pu <- pick(2)
pt_freq$apu <- pick(3)

usethis::use_data(pt_freq, overwrite = TRUE, compress = "bzip2")
