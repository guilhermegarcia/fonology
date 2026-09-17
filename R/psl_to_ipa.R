#' Convert Portuguese Stress Lexicon notation to IPA
#'
#' The PSL encodes transcriptions in an ASCII notation: \code{-} marks syllable
#' boundaries, \code{'} marks stress, \code{E O L N S Z} stand for
#' \code{ɛ ɔ ʎ ɲ ʃ ʒ}, \code{R} is the strong rhotic, \code{r} the tap, and
#' \code{~} marks nasality. This function maps that notation onto the IPA
#' conventions used by \code{ipa()}.
#'
#' Rhotics. With \code{rhotics = "phonemic"} (the default), \code{R} becomes
#' \code{r} and \code{r} becomes \code{ɾ}, i.e. the two contrastive rhotic
#' phonemes. \code{rhotics = "legacy"} reproduces the convention used up to
#' Fonology 1.7.0, where the strong rhotic was written with its [x] allophone
#' and the tap as \code{r}; it exists to validate the conversion against the
#' objects shipped with that version.
#'
#' Nasality. \code{V~} becomes a nasal vowel; in a nasal diphthong \code{V~G}
#' both the vowel and the glide are nasal. A bare \code{~}, which occurs only in
#' the PSL coda columns, is the nasal coda archiphoneme and becomes \code{N}:
#' the PSL treats a nasal monophthong as an oral vowel closed by a nasal coda.
#'
#' Three PSL entries (xênico, xona, xucro) contain a literal \code{x}, which is
#' the orthographic letter copied into the transcription where /ʃ/ is intended.
#' It is converted to \code{ʃ}.
#'
#' The label \code{"None"}, used in the PSL segment columns, and \code{NA} are
#' returned unchanged.
#'
#' @param x A character (or factor) vector in PSL notation
#' @param rhotics \code{"phonemic"} (default) or \code{"legacy"}
#' @return A character vector in IPA
#' @noRd

psl_to_ipa <- function(x, rhotics = c("phonemic", "legacy")) {
  rhotics <- match.arg(rhotics)
  x <- as.character(x)
  keep <- !is.na(x) & x != "None"
  y <- x[keep]

  nasal <- c(a = "\u00e3", e = "\u1ebd", i = "\u0129", o = "\u00f5", u = "\u0169")

  # Nasal diphthong: both the vowel and the glide are nasal.
  for (v in names(nasal)) {
    y <- stringr::str_replace_all(y, paste0(v, "~([wj])"), paste0(nasal[[v]], "\\1\u0303"))
  }
  # Nasal monophthong.
  for (v in names(nasal)) {
    y <- stringr::str_replace_all(y, paste0(v, "~"), nasal[[v]])
  }

  # Orthographic x copied into three PSL transcriptions, standing for /ʃ/.
  y <- stringr::str_replace_all(y, "x", "\u0283")

  # Rhotics. Order matters in the phonemic mapping: the tap must be rewritten
  # before R is lowered to r, or both would end up as the tap.
  if (rhotics == "phonemic") {
    y <- stringr::str_replace_all(y, "r", "\u027e")
    y <- stringr::str_replace_all(y, "R", "r")
  } else {
    y <- stringr::str_replace_all(y, "R", "x")
  }

  y <- chartr("EOLNSZ", "\u025b\u0254\u028e\u0272\u0283\u0292", y)

  # A bare ~ (coda columns) is the nasal archiphoneme. This must follow the
  # chartr() above, which would otherwise turn N into ɲ.
  y <- stringr::str_replace_all(y, "~", "N")

  y <- stringr::str_replace_all(y, "'", "\u02c8")
  y <- stringr::str_replace_all(y, "-", ".")

  x[keep] <- y
  x
}
