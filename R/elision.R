#' Elided clitics in French and Italian
#'
#' French and Italian write an elided function word and its host as one
#' orthographic token (\emph{l'homme}, \emph{dell'acqua}). Removing the
#' apostrophe yields a string no lexicon lists, and which can collide with an
#' unrelated word (\emph{s'en} and \emph{sen}, \emph{c'era} and \emph{cera}).
#' So the host is transcribed on its own and the clitic's IPA is attached to it.
#'
#' In the tables below a dot separates material that stays in its own syllable
#' from the consonant that resyllabifies as the onset of the host.
#' @noRd

.elision_fr <- c(
  c = "s", d = "d", j = "\u0292", l = "l", m = "m", n = "n", qu = "k",
  s = "s", t = "t",
  jusqu = "\u0292ys.k", lorsqu = "l\u0254\u0281s.k", presqu = "p\u0281\u025bs.k",
  puisqu = "p\u0265is.k", quelqu = "k\u025bl.k", quoiqu = "kwa.k"
)

.elision_it <- c(
  c = "t\u0283", ch = "k", d = "d", gl = "\u028e", l = "l", m = "m", n = "n",
  s = "s", t = "t", v = "v",
  all = "al.l", anch = "an.k", bell = "b\u025bl.l", coll = "kol.l", com = "ko.m",
  dall = "dal.l", dell = "del.l", dov = "do.v", grand = "gran.d", mezz = "med.dz",
  nell = "nel.l", nient = "njen.t", quell = "kwel.l", quest = "kwes.t",
  sant = "san.t", senz = "sen.ts", sull = "sul.l", tutt = "tut.t", un = "u.n"
)

# Attaches a clitic's IPA to its host's. Word stress and any secondary stress
# mark the host's first syllable, which now begins with the clitic's onset, so
# the onset goes after the mark.
.join_elision <- function(clitic, host, stressed = FALSE) {
  # Italian monosyllables can be stored unstressed (\u00e8 = \u025b).
  if (stressed) {
    host <- ifelse(stringr::str_detect(host, "\u02c8"), host, paste0("\u02c8", host))
  }
  parts <- stringr::str_match(clitic, "^(?:(.*)\\.)?([^.]*)$")
  syllables <- ifelse(is.na(parts[, 2]), "", paste0(parts[, 2], "."))
  marks <- stringr::str_extract(host, "^[\u02c8\u02cc]?")
  rest <- stringr::str_remove(host, "^[\u02c8\u02cc]")
  paste0(syllables, marks, parts[, 3], rest)
}

# out: current transcriptions. raw: lowercased tokens with apostrophes intact.
# transcribe: the language's own ipa function, applied to the hosts.
.apply_elision <- function(out, raw, skip, table, transcribe, stressed = FALSE) {
  m <- stringr::str_match(raw, "^(\\p{L}+)'(\\p{L}.*)$")
  hit <- !is.na(m[, 1]) & !skip & m[, 2] %in% names(table)
  if (any(hit)) {
    host <- transcribe(m[hit, 3])
    out[hit] <- .join_elision(unname(table[m[hit, 2]]), host, stressed)
  }
  out
}
