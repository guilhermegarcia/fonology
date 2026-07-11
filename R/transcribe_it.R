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
  # English-style <ck> spells a single /k/ in common loanwords.
  word <- stringr::str_replace_all(word, "ck", "k")

  # IMPORTANT: Digraphs involving <h> (ch, gh, sch) must be resolved BEFORE
  # the silent-h deletion rule, or "chiesa" would lose its <h> and "cie" would
  # be misread as the palatal sequence /t\u0283e/ instead of /ke/.

  # 1. SCH + e/i \u2192 sk (hard velar cluster; MUST precede sc and h-removal rules)
  word <- stringr::str_replace_all(word, "sch([ei\u00e8\u00e9\u00ea\u00ec])", "sk\\1")

  # 2. CH + e/i \u2192 k (hard velar; MUST precede c rule and h removal)
  word <- stringr::str_replace_all(word, "ch([ei\u00e8\u00e9\u00ea\u00ec])", "k\\1")

  # 3. GH + e/i \u2192 temporary uppercase G marker (hard velar).
  #    Using 'G' (uppercase) avoids the later g([ei\u00e8\u00e9\u00ea\u00ec])\u2192d\u0292 palatalisation rule,
  #    which only matches lowercase 'g'. The marker is restored to 'g' after
  #    all palatalisation rules have run (see step 15 below).
  word <- stringr::str_replace_all(word, "gh([ei\u00e8\u00e9\u00ea\u00ec])", "G\\1")

  # 4. Silent h \u2014 now safe to delete remaining <h> (none left in ch/gh/sch positions)
  word <- stringr::str_remove_all(word, "h")

  # 4b. X spells /ks/ (extra, xeno-)
  word <- stringr::str_replace_all(word, "x", "ks")

  # 5. QU: qu + vowel \u2192 kw + vowel; remaining q \u2192 k
  word <- stringr::str_replace_all(word, "qu([aeiou\u00e0\u00e1\u00e2\u00e8\u00e9\u00ea\u00ec\u00ed\u00ee\u00f2\u00f3\u00f4\u00f9\u00fa\u00fb])", "kw\\1")
  word <- stringr::str_replace_all(word, "q", "k")

  # 6. GN \u2192 \u0272 (palatal nasal)
  word <- stringr::str_replace_all(word, "gn", "\u0272")

  # 7. GLI: gli + vowel \u2192 \u028e + vowel (drop silent i before another vowel);
  #    remaining gli \u2192 \u028ei (e.g. article "gli", word-final position)
  word <- stringr::str_replace_all(word, "gli([aeiou\u00e0\u00e1\u00e2\u00e8\u00e9\u00ea\u00ec\u00ed\u00ee\u00f2\u00f3\u00f4\u00f9\u00fa\u00fb])", "\u028e\\1")
  word <- stringr::str_replace_all(word, "gli", "\u028ei")

  # 8. SCI + a/o/u \u2192 \u0283 + vowel (drop silent <i> before non-front vowels)
  word <- stringr::str_replace_all(word, "sci([aou\u00e0\u00e1\u00e2\u00f2\u00f3\u00f4\u00f9\u00fa\u00fb])", "\u0283\\1")

  # 9. SC + e/i \u2192 \u0283 (palatal sibilant before front vowels)
  word <- stringr::str_replace_all(word, "sc([ei\u00e8\u00e9\u00ea\u00ec])", "\u0283\\1")

  # 10. SC \u2192 sk (remaining <sc> before a/o/u or consonant, e.g. "scala", "scuola")
  word <- stringr::str_replace_all(word, "sc", "sk")

  # 11. GG + e/i \u2192 dd\u0292 (geminate palatal affricate; MUST precede single-g rule).
  #    When <ggi> precedes a/o/u, the <i> is a silent orthographic marker (like
  #    in <gi>+vowel) and is dropped: "viaggio" \u2192 vjadd\u0292o, not vjadd\u0292io.
  word <- stringr::str_replace_all(word, "ggi([aou\u00e0\u00e1\u00e2\u00f2\u00f3\u00f4\u00f9\u00fa\u00fb])", "dd\u0292\\1")
  word <- stringr::str_replace_all(word, "gg([ei\u00e8\u00e9\u00ea\u00ec])", "dd\u0292\\1")

  # 12. CC + e/i \u2192 tt\u0283 (geminate palatal affricate; MUST precede single-c rule).
  #    Same silent-i rule: <cci> before a/o/u drops the i.
  word <- stringr::str_replace_all(word, "cci([aou\u00e0\u00e1\u00e2\u00f2\u00f3\u00f4\u00f9\u00fa\u00fb])", "tt\u0283\\1")
  word <- stringr::str_replace_all(word, "cc([ei\u00e8\u00e9\u00ea\u00ec])", "tt\u0283\\1")

  # 13. GI + vowel \u2192 d\u0292 + vowel (drop silent <i> before another vowel)
  word <- stringr::str_replace_all(word, "gi([aeiou\u00e0\u00e1\u00e2\u00e8\u00e9\u00ea\u00ec\u00ed\u00ee\u00f2\u00f3\u00f4\u00f9\u00fa\u00fb])", "d\u0292\\1")

  # 14. G + e/i \u2192 d\u0292 (palatal affricate before front vowels)
  word <- stringr::str_replace_all(word, "g([ei\u00e8\u00e9\u00ea\u00ec])", "d\u0292\\1")

  # 15. Restore hard-G placeholder \u2192 lowercase g.
  #    This runs AFTER all g palatalisation rules so the restored 'g' is not
  #    re-palatalized (those rules have already executed).
  word <- stringr::str_replace_all(word, "G", "g")

  # 16. GU + vowel \u2192 gw + vowel (u becomes labial glide before another vowel)
  word <- stringr::str_replace_all(word, "gu([aeiou\u00e0\u00e1\u00e2\u00e8\u00e9\u00ea\u00ec\u00ed\u00ee\u00f2\u00f3\u00f4\u00f9\u00fa\u00fb])", "gw\\1")

  # 16. CI + vowel \u2192 t\u0283 + vowel (drop silent <i> before another vowel)
  word <- stringr::str_replace_all(word, "ci([aeiou\u00e0\u00e1\u00e2\u00e8\u00e9\u00ea\u00ec\u00ed\u00ee\u00f2\u00f3\u00f4\u00f9\u00fa\u00fb])", "t\u0283\\1")

  # 17. C + e/i \u2192 t\u0283 (palatal affricate before front vowels)
  word <- stringr::str_replace_all(word, "c([ei\u00e8\u00e9\u00ea\u00ec])", "t\u0283\\1")

  # 18. Remaining c \u2192 k
  word <- stringr::str_replace_all(word, "c", "k")

  # 19. ZZ: the -izzare verb class is voiced (iddz); other zz \u2192 tts.
  #    MUST precede the single-z rule.
  word <- stringr::str_replace_all(word, "izz(?=[aeio])", "iddZ")
  word <- stringr::str_replace_all(word, "zz", "tts")

  # 19b. Intervocalic z is geminate: t.ts before i (-zione, -zia classes,
  #      464-5 in the Wiktionary lexicon), d.dz elsewhere (azoto-type, 87-12).
  #      Z is a placeholder so the affricate survives the z \u2192 ts rule.
  word <- stringr::str_replace_all(word,
    "([aeiou\u00e0\u00e1\u00e2\u00e8\u00e9\u00ea\u00ec\u00ed\u00ee\u00f2\u00f3\u00f4\u00f9\u00fa\u00fb])z(?=i)",
    "\\1tts"
  )
  word <- stringr::str_replace_all(word,
    "([aeiou\u00e0\u00e1\u00e2\u00e8\u00e9\u00ea\u00ec\u00ed\u00ee\u00f2\u00f3\u00f4\u00f9\u00fa\u00fb])z(?=[aeou\u00e0\u00e8\u00e9\u00ea\u00f2\u00f3\u00f4\u00f9])",
    "\\1ddZ"
  )

  # 20. Word-initial z + vowel \u2192 dz (voiced word-initially in standard Italian).
  #    Placeholder Z keeps the new affricate safe from the z \u2192 ts rule below.
  word <- stringr::str_replace_all(word, "^z([aeiou\u00e0\u00e8\u00e9\u00ea\u00ec\u00f2\u00f3\u00f4\u00f9])", "dZ\\1")

  # 21. Remaining z \u2192 ts; restore the initial affricate
  word <- stringr::str_replace_all(word, "z", "ts")
  word <- stringr::str_replace_all(word, "Z", "z")

  # 21b. s voices before voiced consonants (sbaglio, slavo; 1407-1)
  word <- stringr::str_replace_all(word, "s(?=[bdglmnrv])", "z")

  # 22. Protect geminate ss, apply intervocalic s \u2192 z, then restore ss
  word <- stringr::str_replace_all(word, "ss", "SS")
  word <- stringr::str_replace_all(word,
    "([aeiou\u00e0\u00e1\u00e2\u00e8\u00e9\u00ea\u00ec\u00ed\u00ee\u00f2\u00f3\u00f4\u00f9\u00fa\u00fb])s([aeiou\u00e0\u00e1\u00e2\u00e8\u00e9\u00ea\u00ec\u00ed\u00ee\u00f2\u00f3\u00f4\u00f9\u00fa\u00fb])",
    "\\1z\\2"
  )
  word <- stringr::str_replace_all(word, "SS", "ss")

  # 23b. \u0283, \u0272, \u028e (and intervocalic z affricates) are inherently
  #      geminate between vowels in Italian (lasciare \u2192 la\u0283.\u0283a.re).
  Vg <- "[aeiou\u00e0\u00e1\u00e2\u00e8\u00e9\u00ea\u00ec\u00ed\u00ee\u00f2\u00f3\u00f4\u00f9\u00fa\u00fb]"
  word <- stringr::str_replace_all(word, paste0("(", Vg, ")\u0283(", Vg, "|[jw])"), "\\1\u0283\u0283\\2")
  word <- stringr::str_replace_all(word, paste0("(", Vg, ")\u0272(", Vg, "|[jw])"), "\\1\u0272\u0272\\2")
  word <- stringr::str_replace_all(word, paste0("(", Vg, ")\u028e(", Vg, "|[jw])"), "\\1\u028e\u028e\\2")

  # 24-25. Glide formation \u2014 must happen before syllabification so that
  # onset glides (e.g. "piano" \u2192 /pjano/, "scuola" \u2192 /skwola/) are in the
  # right syllable. Only applies when i/u is flanked by a consonant on the
  # left and a vowel on the right (or word-initially before a vowel).
  #
  # Character class for consonants at this stage:
  #   p b t d k g f v s z m n l r \u0272 \u028e \u014b \u0283 \u0292 (and complex tokens ending in these)
  C_class <- "[pbtdkgfvszmnlr\u0272\u028e\u014b\u0283\u0292]"
  V_class <- "[aeiou\u00e0\u00e1\u00e2\u00e8\u00e9\u00ea\u00ec\u00ed\u00ee\u00f2\u00f3\u00f4\u00f9\u00fa\u00fb]"

  # u after consonant and before vowel \u2192 w (e.g. "scuola" \u2192 "skwola");
  # identical-vowel sequences stay in hiatus (diidrogeni, duumviro)
  word <- stringr::str_replace_all(word,
    paste0("(", C_class, ")u(?!u)(", V_class, ")"),
    "\\1w\\2"
  )
  # i after consonant and before vowel \u2192 j (e.g. "piano" \u2192 "pjano", "chiesa" \u2192 "kjeza")
  word <- stringr::str_replace_all(word,
    paste0("(", C_class, ")i(?!i)(", V_class, ")"),
    "\\1j\\2"
  )

  # i between vowels is a glide (gioia \u2192 d\u0292\u0254ja)
  word <- stringr::str_replace_all(word,
    paste0("(", V_class, ")i(", V_class, ")"),
    "\\1j\\2"
  )

  # ri- prefix stays in hiatus (riesploso \u2192 ri.es, riaffezionare \u2192 ri.af)
  word <- stringr::str_replace_all(word, "^rj", "ri")

  # Word-initial u/i before a vowel \u2192 glide (e.g. "uomo" \u2192 "womo", "ieri" \u2192 "jeri")
  word <- stringr::str_replace_all(word, paste0("^u(", V_class, ")"), "w\\1")
  word <- stringr::str_replace_all(word, paste0("^i(", V_class, ")"), "j\\1")

  # Note: accent marks (\u00e0 \u00e8 \u00e9 \u00ec \u00f2 \u00f3 \u00f9) are intentionally preserved here;
  # stress_it() uses them to locate the stressed syllable before normalizing.

  return(word)
}
