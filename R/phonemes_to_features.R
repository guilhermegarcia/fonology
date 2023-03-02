#' Distinctive feature generator
#'
#' Generates a feature matrix for a given set of phonemes in a given language
#' @param ph The phonemes of interest
#' @param lg The language of interest: English, French, Portuguese, Spanish
#' @return The minimal matrix of features given ph and lg
#' @examples
#' getFeat(ph = c("i", "u"), lg = "english");
#' @importFrom magrittr %>%
#' @export

getFeat = function(ph = c(), lg = "Portuguese"){

  phonemes = "i.y.ɨ.ʉ.ɯ.u.ɪ.ʏ.ʊ.e.ø.ɘ.ɵ.ɤ.o.ɛ.œ.ə.ɜ.ɞ.ʌ.ɔ.ɐ.æ.ɶ.a.ɑ.ɒ.ɛ̃.œ̃.ɔ.j.ɥ.w.p.b.t.d.ʈ.ɖ.c.ɟ.k.ɡ.q.ɢ.ʔ.m.ɱ.n.ɳ.ɲ.ŋ.ɴ.ʙ.r.ʀ.ⱱ.ɾ.ɽ.ɸ.β.f.v.θ.ð.s.z.ʃ.ʒ.ʂ.ʐ.ç.ʝ.x.ɣ.χ.ʁ.ħ.ʕ.h.ɦ.ɬ.ɮ.ʋ.ɹ.ɻ.j.ɰ.l.ɭ.ʎ.ʟ.d͡z.t͡s.t͡ʃ.d͡ʒ"

  phonemes = phonemes %>%
    stringr::str_split(pattern = "\\.") %>%
    unlist()

  vowels = phonemes[1:31]
  semivowels = phonemes[31:33]
  liquids = c("l.r.ɾ.ɽ.l.ɭ.ʎ.ʟ.ɹ.ɻ.ʁ.ʀ") %>% stringr::str_split(pattern = "\\.") %>% unlist()
  nasals = "m.ɱ.n.ɳ.ɲ.ŋ.ɴ" %>% stringr::str_split(pattern = "\\.") %>% unlist()
  fricatives = "ɸ.β.f.v.θ.ð.s.z.ʃ.ʒ.ʂ.ʐ.ç.ʝ.x.ɣ.χ.ʁ.ħ.ʕ.h.ɦ" %>% stringr::str_split(pattern = "\\.") %>% unlist()
  affricates = "d͡z.t͡s.t͡ʃ.d͡ʒ" %>% stringr::str_split(pattern = "\\.") %>% unlist()

  allFeatures = allFeatures %>%
    dplyr::filter(ipa %in% phonemes) %>%
    droplevels() %>%
    dplyr::mutate(approx = ifelse(ipa %in% c(vowels, semivowels, liquids), "+", "-")) %>%
    dplyr::select(ipa, syl, cons, son, approx, cont:hireg)

  # Pick one language to work with:
  portuguese = "a.e.i.o.u.ɛ.ɔ.j.w.p.b.t.d.k.g.f.v.s.z.ʃ.ʒ.m.n.ɲ.l.r.ɾ.ʎ" %>%
    stringr::str_split(pattern = "\\.") %>%
    unlist()

  french = c("a.e.ø.ɑ.i.y.o.u.ɛ.ɔ.ə.œ.ɛ̃.œ̃.ɔ̃.ɑ̃.p.b.t.d.k.g.f.v.s.z.ʃ.ʒ.ʁ.m.ɱ.n.ɲ.ŋ.l.w.j.ɥ") %>%
    stringr::str_split(pattern = "\\.") %>%
    unlist()

  english = c("a.e.ɑ.i.o.u.ɛ.ɔ.ə.ɪ.ʊ.æ.ʌ.p.b.f.k.g.v.t.d.s.z.ʃ.ʒ.t͡ʃ.d͡ʒ.θ.ð.m.n.ŋ.h.w.j.ɹ.l") %>%
    stringr::str_split(pattern = "\\.") %>%
    unlist()

  spanish = c("a.e.ɑ.i.o.u.p.b.f.v.t.d.k.g.s.z.t͡ʃ.θ.m.ɲ.w.j.l.r.ɾ.ʎ.x.ʝ") %>%
    stringr::str_split(pattern = "\\.") %>%
    unlist()

  # Select language:
  targetLanguage = eval(parse(text = stringr::str_to_lower(lg)))


  availableLg = c("portuguese", "french", "english", "spanish")

  if(!stringr::str_to_lower(lg) %in% availableLg){
    stop("Language not supported (or misspelled).")
  }

  # Select features for phonemes:
  targetF = allFeatures %>%
    dplyr::filter(ipa %in% targetLanguage) %>%
    droplevels()


  # Reduce number of features (remove all that are useless):
  targetF = targetF %>%
    dplyr::select(dplyr::where(~dplyr::n_distinct(.) > 1))

  # Function to test if all elements are the same:
  same = function(values = c()){
    if(length(unique(values)) == 1){
      return(values[1])
    } else {
      return(NA)
    }
  }

  chosenPh = ph

  for(phoneme in 1:length(chosenPh)){
    if(chosenPh[phoneme] == "tʃ"){
      chosenPh[phoneme] = "t͡ʃ"
    } else if(chosenPh[phoneme] == "dʒ"){
      chosenPh[phoneme] = "d͡ʒ"
    } else {
      chosenPh[phoneme] = chosenPh[phoneme]
    }
  }

  if(!all(chosenPh %in% targetLanguage)){
    stop("Input doesn't match phonemic inventory in language.")
  }

  chosenPhF = targetF %>%
    dplyr::filter(ipa %in% chosenPh) %>%
    droplevels()

  # Pick intersection:

  all_cols = names(chosenPhF)[-1]

  uniqueF = chosenPhF %>%
    dplyr::select(-ipa) %>%
    dplyr::summarize(dplyr::across(.cols = dplyr::all_of(all_cols), .fns = same)) %>%
    dplyr::select(dplyr::where(~sum(!is.na(.x)) > 0))

  # combinedFeatures = targetF %>% right_join(uniqueF)

  combinedFeatures = merge(targetF, uniqueF, all.y = TRUE)

  if(nrow(combinedFeatures) > length(chosenPh)){
    return("Not a natural class in this language.")
  } else {


    totalLength = ncol(uniqueF)

    minimalMatrix = FALSE

    for(i in 1:totalLength){

      x1 = gtools::combinations(totalLength, i)

      x2 = as.data.frame(x1) %>%
        dplyr::as_tibble()

      for(j in seq_along(1:nrow(x2))){

        x3 = x2 %>%
          dplyr::slice(j) %>%
          unlist()

        x4 = uniqueF %>% dplyr::select(dplyr::all_of(as.vector(x3)))
        # x5 = targetF %>% right_join(x4)
        x5 = merge(targetF, x4, all.y = TRUE)

        if(nrow(x5) == nrow(chosenPhF)){
          # print("Found minimal matrix:")

          feats = c()

          for(i in 1:length(names(x4))){
            feats[length(feats)+1] =  stringr::str_c(x4[1,i], names(x4[i]))
          }

          minimalMatrix = TRUE
          return(feats)

          break
        }

      }

      if(isTRUE(minimalMatrix)){
        break
      }
    }
  }
}
