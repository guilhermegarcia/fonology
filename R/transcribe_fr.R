#' Phonemic transcriber for French
#'
#' Phonemically transcribes a given word in French. The function
#' works best with monomorphemic words. Polymorphemic words may also
#' work well, depending on how predictable they are phonological and orthographic factors.
#' @param word The string of interest in its orthographic form
#' @noRd
#' @return The transcribed word


transcribe_fr <- function(word) {
  word <- word |>
    stringr::str_to_lower()
  word <- stringr::str_replace_all(word, pattern = "[:punct:]", replacement = "")
  word
  ##### REGLES DE TRANSCRIPTION #####
  ##### word UNIQUES (EXEPTIONS)
  word <- stringr::str_replace_all(word, pattern = "monsieur", replacement = "MSWSJ\u00f8")
  word <- stringr::str_replace_all(word, pattern = "yeux", replacement = "ZJ\u00f8")
  word <- stringr::str_replace_all(word, pattern = "hier", replacement = "IJAIr")
  word <- stringr::str_replace_all(word, pattern = "cactus", replacement = "KAKTYS")
  word <- stringr::str_replace_all(word, pattern = "temps", replacement = "T\u0251\u0303")

  #### VOYELLES, SEMI-VOYELLES ET NASALES ####
  ## Gestion des diacritiques + quelques terminaisons
  # E
  word <- stringr::str_replace_all(word, pattern = "\u00e9es|\u00e9e|\u00e9s$|\u00e9s(?= )|\u00e9|ers$|ers(?= )|er$|er(?= )|^et$|(?<= )et(?= )|(?<=i)ed$|(?<=i)ed(?= )|ai$|ai(?= )|ez$|ez(?= )", replacement = "E")
  # i
  word <- stringr::str_replace_all(word, pattern = "\u00ee|\u00ef|ies$|ie$|ies(?= )|ie(?= )|it$|it(?= )|its$|its(?= )|is$|is(?= )", replacement = "i")
  # AI
  word <- stringr::str_replace_all(word, pattern = "\u00e8s$|\u00e8s(?= )|\u00e8|\u00eats$|\u00eats(?= )|\u00ea|\u00eb|aient$|aient(?= )|aix$|aix(?= )|a\u00ee|ai(?!l)|ei(?!l)|ects$|ect$|ects(?= )|ect(?= )|est$|est(?= )|et$|et(?= )|^es$|(?<= )es(?= )", replacement = "AI")
  # Y
  word <- stringr::str_replace_all(word, pattern = "\u00fb|\u00fce|\u00fc|ues$|ues(?= )|ue$|ue(?= )|uts$|uts(?= )|ut$|ut(?= )|us$|us(?= )", replacement = "Y")
  # c deveient s devant eau
  word <- stringr::str_replace_all(word, pattern = "c(?=e)", replacement = "s")
  # o
  word <- stringr::str_replace_all(word, pattern = "\u00f4|eaux$|eaux(?= )|aulx$|aulx(?= )|ault$|ault(?= )|aux$|aux(?= )|eau|au", replacement = "o")
  # a (anterieur)
  word <- stringr::str_replace_all(word, pattern = "\u00e0|as$|as(?= )", replacement = "a")
  # a (posterieur)
  word <- stringr::str_replace_all(word, pattern = "\u00e2", replacement = "\u0251")
  # ɔ
  word <- stringr::str_replace_all(word, pattern = "o(?=m)", replacement = "\u0254")
  # Y (u -> Y)
  word <- stringr::str_replace_all(word, pattern = "\u00f9|u", replacement = "Y")

  ## Gestion des marques complexe
  # U
  word <- stringr::str_replace_all(word, pattern = "oYts$|oYts(?= )|oYt$|oYt(?= )|oYs$|oYs(?= )|oYps$|oYp$|oYps(?= )|oYp(?= )|oY", replacement = "U")
  # OE
  word <- stringr::str_replace_all(word, pattern = "oeY|eY(?=[rl])|\u0153Y|oe", replacement = "OE")
  # ø
  word <- stringr::str_replace_all(word, pattern = "eYx|eY", replacement = "\u00f8")
  # WaJ
  word <- stringr::str_replace_all(word, pattern = "oy", replacement = "WaJ")
  # Wa
  word <- stringr::str_replace_all(word, pattern = "oix$|oix(?= )|oigts$|oigts(?= )|oigt$|oigt(?= )|oi|Ua", replacement = "Wa")
  # ɥi
  word <- stringr::str_replace_all(word, pattern = "Yi", replacement = "\u0265i")
  ## Gestion des voyelles nasales + N (intervocalique)
  # N
  word <- stringr::str_replace_all(word, pattern = "(?<=[AEIOYUa\u0251eio\u0254\u00f8])n(?=[AEIOYUa\u0251eio\u0254\u00f8])|nn", replacement = "N")
  # Wɛ̃
  word <- stringr::str_replace_all(word, pattern = "Wans|Wang|Wan", replacement = "W\u025b\u0303")
  # Jɛ̃
  word <- stringr::str_replace_all(word, pattern = "iens$|iens(?= )|ien", replacement = "J\u025b\u0303")
  # Jɛ
  word <- stringr::str_replace_all(word, pattern = "ie(?=N)", replacement = "JAI")
  # ɛ̃
  word <- stringr::str_replace_all(word, pattern = "AIn(?!es)|AIn(?!e)|ins$|ins(?= )|eins$|eins(?= )|ein|in|im(?=[bmp])|ym(?=[bmp])", replacement = "\u025b\u0303")
  # œ̃
  word <- stringr::str_replace_all(word, pattern = "Yn(?!es)|Yn(?!e)", replacement = "\u0153\u0303")
  # ɔ̃
  word <- stringr::str_replace_all(word, pattern = "onts$|onts(?= )|ont$|ont(?= )|ons$|ons(?= )|ong$|ong(?= )|on(?![nh])|\u0254m(?![me])", replacement = "\u0254\u0303")
  # T
  word <- stringr::str_replace_all(word, pattern = "t(?=[AEIOYUWa\u0251eio\u0254\u00f8])", replacement = "T")
  # z
  word <- stringr::str_replace_all(word, pattern = "(?<=[AEIOYUWa\u0251eio\u0254\u00f8])s(?=ent)", replacement = "z")
  # mɑ̃
  # word <- stringr::str_replace_all(word, pattern = "ment$|ment(?= )", replacement = "mɑ̃")
  word <- stringr::str_replace_all(word, pattern = "amment|emment", replacement = "a\u00a7m\u0251\u0303")

  # dɑ̃
  word <- stringr::str_replace_all(word, pattern = "dent$|dent(?= )", replacement = "d\u0251\u0303")
  # pɑ̃
  word <- stringr::str_replace_all(word, pattern = "pent$|pent(?= )", replacement = "p\u0251\u0303")
  # apocope ent
  word <- stringr::str_replace_all(word, pattern = "ent$|ent(?= )", replacement = "")
  # ɑ̃
  # Fix adverbs "-amment", "-emment" before nasalizing vowel
  word <- stringr::str_replace_all(word, pattern = "ands$|ands(?= )|and$|and(?= )|ants$|ants(?= )|ant$|ant(?= )|ang$|ang(?= )|ans$|ans(?= )|amps$|amps(?= )|amp$|amp(?= )|am(?=[bmp])|em(?=[bmp])|an(?!n)|ents$|ents(?= )|en", replacement = "\u0251\u0303")
  word <- stringr::str_replace_all(word, pattern = "\u00a7", replacement = "")

  #### CONSONNES ####
  # ɲ
  word <- stringr::str_replace_all(word, pattern = "gn", replacement = "\u0272") # ? gnes$|gnes(?= )|gne$|gne(?= )|
  # G
  word <- stringr::str_replace_all(word, pattern = "g(?=[aor\u0254\u0303\u0254Uml])|gY|g\u0265|gg", replacement = "G")
  # ʒ
  word <- stringr::str_replace_all(word, pattern = "g|j", replacement = "\u0292")
  # ʃ
  word <- stringr::str_replace_all(word, pattern = "ch", replacement = "\u0283")
  # il (ill apres v)
  word <- stringr::str_replace_all(word, pattern = "(?<=v)ill|ils$|ils(?= )", replacement = "il")
  # AI
  word <- stringr::str_replace_all(word, pattern = "(?<=^[lmsTDtd])es$|(?<= [lmsTDtd])es(?= )|et(?=T)", replacement = "AI")
  # ɔ
  word <- stringr::str_replace_all(word, pattern = "ot(?=T)", replacement = "\u0254")
  # SW
  word <- stringr::str_replace_all(word, pattern = "(?<=^[klmsTD\u0292n])e$|(?<= [klmsTD\u0292n])e(?= )|(?<=^[klmsTD\u0292n])e(?= )|(?<= [klmsTD\u0292n])e$", replacement = "SW")
  # quelques apocopes
  word <- stringr::str_replace_all(word, pattern = "h|es$|es(?= )|ts$|ds$|t$|d$|ts(?= )|ds(?= )|t(?= )|d(?= )", replacement = "")
  # AI (Transfer de e vers AI devant [lrsx])
  word <- stringr::str_replace_all(word, pattern = "e(?=[rsx])|(?<!v)e(?=l)|(?<=s)e(?=p)|(?<=[\u0292v])e(?=k)", replacement = "AI")
  # AIJ
  word <- stringr::str_replace_all(word, pattern = "eill|eils|eil", replacement = "AIJ")
  # øil
  word <- stringr::str_replace_all(word, pattern = "(?<=\u00f8)ill|(?<=a)il$|(?<=a)il(?= )", replacement = "J")
  # iJ
  word <- stringr::str_replace_all(word, pattern = "ill", replacement = "iJ")
  # sJ
  word <- stringr::str_replace_all(word, pattern = "Ti(?=\u0254\u0303$)|Ti(?=\u0254\u0303 )|Ti(?=\u0254)|Ti(?=\u0254)", replacement = "sJ")
  # J
  word <- stringr::str_replace_all(word, pattern = "(?<=OE)il|i(?=[\u0254\u0303\u0251\u0303AE\u00f8o])", replacement = "J")
  word <- stringr::str_replace_all(word, pattern = "(?<=[Ua])iJ", replacement = "J")
  # Wi
  word <- stringr::str_replace_all(word, pattern = "(?<!G)Ui", replacement = "Wi")
  # d'autres apocopes
  word <- stringr::str_replace_all(word, pattern = "e$|e(?= )|(?<=[rl])s$|(?<=[rl])s(?= )|(?<=wa)x", replacement = "")
  # z
  word <- stringr::str_replace_all(word, pattern = "(?<=[AEIOYUWa\u0251eio\u0254\u00f8])s(?=[AEIOYUWa\u0251eio\u0254\u00f8])", replacement = "z")
  # s
  word <- stringr::str_replace_all(word, pattern = "ss|c(?=[AEei\u00f8\u025b\u0303j])|\u00e7", replacement = "s")
  # i
  word <- stringr::str_replace_all(word, pattern = "y", replacement = "i")
  # k
  word <- stringr::str_replace_all(word, pattern = "qY|q\u0265|ch(?=l)|c(?!h)|q", replacement = "k")
  # x devient s
  word <- stringr::str_replace_all(word, pattern = "(?<=Di|si)x$|(?<=Di|si)x(?= )", replacement = "s")
  # ks
  word <- stringr::str_replace_all(word, pattern = "x", replacement = "ks")
  # consonnes doubles restantes


  word <- stringr::str_replace_all(word, pattern = "pp", replacement = "p")
  word <- stringr::str_replace_all(word, pattern = "mm", replacement = "m")
  word <- stringr::str_replace_all(word, pattern = "ll", replacement = "l")
  word <- stringr::str_replace_all(word, pattern = "rr", replacement = "\u0281")
  word <- stringr::str_replace_all(word, pattern = "r", replacement = "\u0281")
  word <- stringr::str_replace_all(word, pattern = "ff|ph", replacement = "f")
  word <- stringr::str_replace_all(word, pattern = "bb", replacement = "b")
  word <- stringr::str_replace_all(word, pattern = "kk", replacement = "k")

  # Before removing TT sequences:
  # e before tt explicitly becomes ɛ
  word <- stringr::str_replace_all(word, pattern = "e(?=tt)", replacement = "AI")
  word <- stringr::str_replace_all(word, pattern = "TT|tt", replacement = "t")
  # SW
  word <- stringr::str_replace_all(word, pattern = "e", replacement = "SW")
  # epenthese de SW apres k seul
  word <- stringr::str_replace_all(word, pattern = "^k$|(?<= )k(?= )", replacement = "kSW")
  word <- stringr::str_replace_all(word, pattern = "^d$|(?<= )d(?= )", replacement = "dSW")


  #### FIN DE LA TRANSCRIPTION ####

  #### Transfer des MAJUSCULE en minuscule ####
  ### MAJ -> symbole
  word <- stringr::str_replace_all(word, pattern = "OE", replacement = "\u0153")
  word <- stringr::str_replace_all(word, pattern = "SW", replacement = "\u0259")
  word <- stringr::str_replace_all(word, pattern = "AI", replacement = "\u025b")

  ### MAJ -> min
  word <- word |>
    stringr::str_to_lower()



  return(word)
}
