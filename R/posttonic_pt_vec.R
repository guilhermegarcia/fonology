#' Posttonic adjustments for Portuguese
#'
#' Post-stress pass applied to regex-derived (out-of-vocabulary) forms so they
#' match the conventions of the Portuguese Stress Lexicon: (1) posttonic vowel
#' hiatus resolves to glides (fal\u00eancia-type -ia -> ja, \u00eddeo-type -eo -> ew);
#' (2) stressed mid vowels are lowered in the contexts where the PSL shows a
#' clear majority pattern (e.g., proparoxytones, paroxytones ending in -e).
#' @param word A vector of transcribed, syllabified, stressed strings
#' @return The adjusted transcriptions
#' @noRd

posttonic_pt_vec <- function(word, ortho = word) {
  mapply(function(x, wd_ortho) {
    if (is.na(x) || !stringr::str_detect(x, "\u02c8")) {
      return(x)
    }

    # Posttonic glide formation: hiatus after the stressed syllable
    m <- stringr::str_match(x, "^(.*\u02c8[^.]*)\\.(.+)$")
    if (!is.na(m[1, 1])) {
      post <- m[1, 3] |>
        stringr::str_replace_all("i\\.([aeou])", "j\\1") |>
        stringr::str_replace_all("e\\.o(s?)$", "ew\\1") |>
        stringr::str_replace_all("e\\.([au])", "j\\1") |>
        stringr::str_replace_all("u\\.([ao])", "w\\1") |>
        stringr::str_replace_all("o\\.a(s?)$", "wa\\1")
      x <- stringr::str_c(m[1, 2], ".", post)
    }

    # Stressed mid-vowel lowering (PSL majority contexts, oral syllables only)
    syls <- stringr::str_split(x, stringr::fixed("."))[[1]]
    k <- which(stringr::str_detect(syls, "\u02c8"))[1]
    n <- length(syls)
    pos <- n - k

    if (pos %in% 1:2) {
      s <- syls[k]
      nasal <- stringr::str_detect(s, "[eo][nm]") ||
        stringr::str_detect(s, "[\u00e3\u00f5]")

      next_nasal <- k < n && stringr::str_detect(syls[k + 1], "^[nm\u0272]") &&
        stringr::str_detect(s, "[aeiou]$")
      after_glide <- stringr::str_detect(s, "[jw][eo]")

      if (!nasal && !next_nasal && !after_glide) {
        fin_syl <- syls[n]
        fin <- stringr::str_remove(fin_syl, "s$") |>
          stringr::str_sub(-1)
        glide_fin <- stringr::str_detect(fin_syl, "[jw][aeou]s?$")

        low_e <- (pos == 2 && fin %in% c("a", "o")) ||
          (pos == 1 && glide_fin && fin == "a") ||
          (pos == 1 && stringr::str_detect(wd_ortho, "(ela|eca|eba|eco|ermo)s?$"))
        low_o <- (pos == 2 && fin %in% c("a", "e", "o")) ||
          (pos == 1 && fin %in% c("e", "w")) ||
          (pos == 1 && glide_fin && fin == "o") ||
          (pos == 1 && stringr::str_detect(wd_ortho, "(oca|ota|ola|oba|osa|orfo|ormo)s?$"))

        # Circumflex accents mark closed mid vowels; never lower those
        if (stringr::str_detect(wd_ortho, "\u00ea")) low_e <- FALSE
        if (stringr::str_detect(wd_ortho, "\u00f4")) low_o <- FALSE

        if (low_e) s <- stringr::str_replace(s, "e(?![jw])", "\u025b")
        if (low_o) s <- stringr::str_replace(s, "o(?![jw])", "\u0254")
        syls[k] <- s
      }
    }

    stringr::str_c(syls, collapse = ".")
  }, word, ortho, USE.NAMES = FALSE)
}
