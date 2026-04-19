#' Syllabifier for Italian
#'
#' Syllabifies a given word in Standard Italian using onset maximization.
#' @param word The string of interest using IPA phonemic transcription
#' @noRd
#' @return The syllabified version of the string in question

syllabify_it <- function(word) {

  # Step 1: Protect BASE affricates only (NOT their geminate forms).
  #
  # Geminate affricates (ddʒ, ttʃ, tts) are intentionally left with their
  # first consonant exposed so the split rules below can move it to the coda.
  # For example, ddʒ becomes d+DJ: the 'd' participates in the dot-placement
  # logic normally, then step 5a moves it to the preceding coda.
  #
  # Order: longest first to prevent partial matches.
  word <- stringr::str_replace_all(word, "t\u0283", "TC")   # tʃ  → TC
  word <- stringr::str_replace_all(word, "d\u0292", "DJ")   # dʒ  → DJ
  word <- stringr::str_replace_all(word, "dz", "DZ")        # dz  → DZ
  word <- stringr::str_replace_all(word, "ts", "TZ")        # ts  → TZ
  # After the above: ddʒ → dDJ, ttʃ → tTC, tts → tTZ (first consonant exposed)

  # Step 2: Insert syllable boundary after every vowel (onset maximization)
  word <- stringr::str_replace_all(word,
    "([aeiou\u00e0\u00e1\u00e2\u00e8\u00e9\u00ea\u00ec\u00ed\u00ee\u00f2\u00f3\u00f4\u00f9\u00fa\u00fb])",
    "\\1."
  )

  # Step 3: Remove trailing dot
  word <- stringr::str_remove_all(word, "\\.$")

  # Step 4: Move glides back to their nucleus (diphthong protection)
  # e.g. V.jC → Vj.C  and  V.wC → Vw.C
  word <- stringr::str_replace_all(word,
    "([aeiou\u00e0\u00e1\u00e2\u00e8\u00e9\u00ea\u00ec\u00ed\u00ee\u00f2\u00f3\u00f4\u00f9\u00fa\u00fb])\\.([jw])",
    "\\1\\2."
  )

  # Step 4b: Falling diphthongs with postvocalic i/u remain tautosyllabic.
  word <- stringr::str_replace_all(word,
    "([aeo\u00e0\u00e1\u00e2\u00e8\u00e9\u00ea\u00f2\u00f3\u00f4])\\.([iu\u00ec\u00ed\u00ee\u00f9\u00fa\u00fb])",
    "\\1\\2."
  )

  # Step 5a: Geminate affricate split — the exposed first consonant moves to
  # the preceding coda; the base affricate becomes the onset of the next syllable.
  #   ddʒ → d.dʒ  (stored as  \.d(DJ)  → d.DJ)
  #   ttʃ → t.tʃ  (stored as  \.t(TC)  → t.TC)
  #   tts → t.ts  (stored as  \.t(TZ)  → t.TZ)
  word <- stringr::str_replace_all(word, "\\.d(DJ)", "d.\\1")
  word <- stringr::str_replace_all(word, "\\.t(TC)", "t.\\1")
  word <- stringr::str_replace_all(word, "\\.t(TZ)", "t.\\1")

  # Step 5b: Plain geminate consonant split — first copy goes to coda, second to onset.
  word <- stringr::str_replace_all(word,
    "\\.([pbtdkgfvszmnlr\u0272\u028e\u014b])(\\1)",
    "\\1.\\2"
  )

  # Step 6a: Sonorants (l, m, n, r, ɲ, ʎ, ŋ) followed by another consonant
  # → sonorant moves to coda of preceding syllable.
  # Do NOT apply this before glides j/w: clusters such as lj and nj belong
  # to the following onset in forms like "italiano".
  word <- stringr::str_replace_all(word,
    "\\.([lmnr\u0272\u028e\u014b])([^aeiou\u00e0\u00e1\u00e8\u00e9\u00ec\u00ed\u00f2\u00f3\u00f9\u00fajw.])",
    "\\1.\\2"
  )

  # Step 6b: s before a consonant → s goes to preceding coda
  word <- stringr::str_replace_all(word,
    "\\.s([pbtdkgfv])",
    "s.\\1"
  )

  # Step 6c: Valid obstruent/fricative + liquid onset clusters stay together.
  # pr, pl, br, bl, tr, dr, kr, kl, gr, gl, fr, fl are all valid Italian onsets.
  word <- stringr::str_replace_all(word,
    "([pbtdkgf])\\.([rl])",
    ".\\1\\2"
  )

  # Step 7: Restore base affricate tokens (reverse of Step 1)
  word <- stringr::str_replace_all(word, "TC", "t\u0283")
  word <- stringr::str_replace_all(word, "DJ", "d\u0292")
  word <- stringr::str_replace_all(word, "DZ", "dz")
  word <- stringr::str_replace_all(word, "TZ", "ts")

  # Step 8: Merge word-final nucleus-less syllable back into preceding coda.
  # Loanwords ending in consonant(s) (e.g. "thriller" → "tril.le.r") get a
  # stranded final segment from the vowel-dot insertion in step 2. Remove the
  # dot before any word-final sequence that contains no vowel at all.
  word <- stringr::str_replace_all(word,
    "\\.([^aeiou\u00e0\u00e1\u00e2\u00e8\u00e9\u00ea\u00ec\u00ed\u00ee\u00f2\u00f3\u00f4\u00f9\u00fa\u00fb\\.]+)$",
    "\\1"
  )

  # Step 9: Clean up double dots and trailing dot
  word <- stringr::str_replace_all(word, "\\.\\.", ".")
  word <- stringr::str_remove_all(word, "\\.$")

  return(word)
}
