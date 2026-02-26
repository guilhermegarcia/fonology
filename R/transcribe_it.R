#' Phonemic transcriber for Italian
#'
#' Phonemically transcribes a given word in Standard Italian. The function
#' works best with monomorphemic words. Polymorphemic words may also
#' work well, depending on how predictable stress is on the basis of
#' phonological and orthographic factors.
#' @param word The string of interest in its orthographic form
#' @noRd
#' @return The transcribed word

transcribe_it <- function(word) {

  # IMPORTANT: Digraphs involving <h> (ch, gh, sch) must be resolved BEFORE
  # the silent-h deletion rule, or "chiesa" would lose its <h> and "cie" would
  # be misread as the palatal sequence /tʃe/ instead of /ke/.

  # 1. SCH + e/i → sk (hard velar cluster; MUST precede sc and h-removal rules)
  word <- stringr::str_replace_all(word, "sch([ei\u00e8\u00e9\u00ea\u00ec])", "sk\\1")

  # 2. CH + e/i → k (hard velar; MUST precede c rule and h removal)
  word <- stringr::str_replace_all(word, "ch([ei\u00e8\u00e9\u00ea\u00ec])", "k\\1")

  # 3. GH + e/i → temporary uppercase G marker (hard velar).
  #    Using 'G' (uppercase) avoids the later g([ei\u00e8\u00e9\u00ea\u00ec])→dʒ palatalisation rule,
  #    which only matches lowercase 'g'. The marker is restored to 'g' after
  #    all palatalisation rules have run (see step 15 below).
  word <- stringr::str_replace_all(word, "gh([ei\u00e8\u00e9\u00ea\u00ec])", "G\\1")

  # 4. Silent h — now safe to delete remaining <h> (none left in ch/gh/sch positions)
  word <- stringr::str_remove_all(word, "h")

  # 5. QU: qu + vowel → kw + vowel; remaining q → k
  word <- stringr::str_replace_all(word, "qu([aeiou\u00e0\u00e1\u00e2\u00e8\u00e9\u00ea\u00ec\u00ed\u00ee\u00f2\u00f3\u00f4\u00f9\u00fa\u00fb])", "kw\\1")
  word <- stringr::str_replace_all(word, "q", "k")

  # 6. GN → ɲ (palatal nasal)
  word <- stringr::str_replace_all(word, "gn", "\u0272")

  # 7. GLI: gli + vowel → ʎ + vowel (drop silent i before another vowel);
  #    remaining gli → ʎi (e.g. article "gli", word-final position)
  word <- stringr::str_replace_all(word, "gli([aeiou\u00e0\u00e1\u00e2\u00e8\u00e9\u00ea\u00ec\u00ed\u00ee\u00f2\u00f3\u00f4\u00f9\u00fa\u00fb])", "\u028e\\1")
  word <- stringr::str_replace_all(word, "gli", "\u028ei")

  # 8. SCI + a/o/u → ʃ + vowel (drop silent <i> before non-front vowels)
  word <- stringr::str_replace_all(word, "sci([aou\u00e0\u00e1\u00e2\u00f2\u00f3\u00f4\u00f9\u00fa\u00fb])", "\u0283\\1")

  # 9. SC + e/i → ʃ (palatal sibilant before front vowels)
  word <- stringr::str_replace_all(word, "sc([ei\u00e8\u00e9\u00ea\u00ec])", "\u0283\\1")

  # 10. SC → sk (remaining <sc> before a/o/u or consonant, e.g. "scala", "scuola")
  word <- stringr::str_replace_all(word, "sc", "sk")

  # 11. GG + e/i → ddʒ (geminate palatal affricate; MUST precede single-g rule).
  #    When <ggi> precedes a/o/u, the <i> is a silent orthographic marker (like
  #    in <gi>+vowel) and is dropped: "viaggio" → vjaddʒo, not vjaddʒio.
  word <- stringr::str_replace_all(word, "ggi([aou\u00e0\u00e1\u00e2\u00f2\u00f3\u00f4\u00f9\u00fa\u00fb])", "dd\u0292\\1")
  word <- stringr::str_replace_all(word, "gg([ei\u00e8\u00e9\u00ea\u00ec])", "dd\u0292\\1")

  # 12. CC + e/i → ttʃ (geminate palatal affricate; MUST precede single-c rule).
  #    Same silent-i rule: <cci> before a/o/u drops the i.
  word <- stringr::str_replace_all(word, "cci([aou\u00e0\u00e1\u00e2\u00f2\u00f3\u00f4\u00f9\u00fa\u00fb])", "tt\u0283\\1")
  word <- stringr::str_replace_all(word, "cc([ei\u00e8\u00e9\u00ea\u00ec])", "tt\u0283\\1")

  # 13. GI + vowel → dʒ + vowel (drop silent <i> before another vowel)
  word <- stringr::str_replace_all(word, "gi([aeiou\u00e0\u00e1\u00e2\u00e8\u00e9\u00ea\u00ec\u00ed\u00ee\u00f2\u00f3\u00f4\u00f9\u00fa\u00fb])", "d\u0292\\1")

  # 14. G + e/i → dʒ (palatal affricate before front vowels)
  word <- stringr::str_replace_all(word, "g([ei\u00e8\u00e9\u00ea\u00ec])", "d\u0292\\1")

  # 15. Restore hard-G placeholder → lowercase g.
  #    This runs AFTER all g palatalisation rules so the restored 'g' is not
  #    re-palatalized (those rules have already executed).
  word <- stringr::str_replace_all(word, "G", "g")

  # 16. GU + vowel → gw + vowel (u becomes labial glide before another vowel)
  word <- stringr::str_replace_all(word, "gu([aeiou\u00e0\u00e1\u00e2\u00e8\u00e9\u00ea\u00ec\u00ed\u00ee\u00f2\u00f3\u00f4\u00f9\u00fa\u00fb])", "gw\\1")

  # 16. CI + vowel → tʃ + vowel (drop silent <i> before another vowel)
  word <- stringr::str_replace_all(word, "ci([aeiou\u00e0\u00e1\u00e2\u00e8\u00e9\u00ea\u00ec\u00ed\u00ee\u00f2\u00f3\u00f4\u00f9\u00fa\u00fb])", "t\u0283\\1")

  # 17. C + e/i → tʃ (palatal affricate before front vowels)
  word <- stringr::str_replace_all(word, "c([ei\u00e8\u00e9\u00ea\u00ec])", "t\u0283\\1")

  # 18. Remaining c → k
  word <- stringr::str_replace_all(word, "c", "k")

  # 19. ZZ → tts (geminate affricate; MUST precede single-z rule)
  word <- stringr::str_replace_all(word, "zz", "tts")

  # 20. Word-initial z + vowel → dz (voiced word-initially in standard Italian)
  word <- stringr::str_replace_all(word, "^z([aeiou\u00e0\u00e8\u00e9\u00ea\u00ec\u00f2\u00f3\u00f4\u00f9])", "dz\\1")

  # 21. Remaining z → ts
  word <- stringr::str_replace_all(word, "z", "ts")

  # 22. Protect geminate ss, apply intervocalic s → z, then restore ss
  word <- stringr::str_replace_all(word, "ss", "SS")
  word <- stringr::str_replace_all(word,
    "([aeiou\u00e0\u00e1\u00e2\u00e8\u00e9\u00ea\u00ec\u00ed\u00ee\u00f2\u00f3\u00f4\u00f9\u00fa\u00fb])s([aeiou\u00e0\u00e1\u00e2\u00e8\u00e9\u00ea\u00ec\u00ed\u00ee\u00f2\u00f3\u00f4\u00f9\u00fa\u00fb])",
    "\\1z\\2"
  )
  word <- stringr::str_replace_all(word, "SS", "ss")

  # 23. Nasal assimilation: n before velar stop → ŋ
  word <- stringr::str_replace_all(word, "n([kg])", "\u014b\\1")

  # 24-25. Glide formation — must happen before syllabification so that
  # onset glides (e.g. "piano" → /pjano/, "scuola" → /skwola/) are in the
  # right syllable. Only applies when i/u is flanked by a consonant on the
  # left and a vowel on the right (or word-initially before a vowel).
  #
  # Character class for consonants at this stage:
  #   p b t d k g f v s z m n l r ɲ ʎ ŋ ʃ ʒ (and complex tokens ending in these)
  C_class <- "[pbtdkgfvszmnlr\u0272\u028e\u014b\u0283\u0292]"
  V_class <- "[aeiou\u00e0\u00e1\u00e2\u00e8\u00e9\u00ea\u00ec\u00ed\u00ee\u00f2\u00f3\u00f4\u00f9\u00fa\u00fb]"

  # u after consonant and before vowel → w (e.g. "scuola" → "skwola")
  word <- stringr::str_replace_all(word,
    paste0("(", C_class, ")u(", V_class, ")"),
    "\\1w\\2"
  )
  # i after consonant and before vowel → j (e.g. "piano" → "pjano", "chiesa" → "kjeza")
  word <- stringr::str_replace_all(word,
    paste0("(", C_class, ")i(", V_class, ")"),
    "\\1j\\2"
  )

  # Word-initial u/i before a vowel → glide (e.g. "uomo" → "womo", "ieri" → "jeri")
  word <- stringr::str_replace_all(word, paste0("^u(", V_class, ")"), "w\\1")
  word <- stringr::str_replace_all(word, paste0("^i(", V_class, ")"), "j\\1")

  # Note: accent marks (à è é ì ò ó ù) are intentionally preserved here;
  # stress_it() uses them to locate the stressed syllable before normalizing.

  return(word)
}
