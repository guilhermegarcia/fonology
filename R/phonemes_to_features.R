#' Distinctive feature generator
#'
#' Generates a feature matrix for a given set of phonemes in a given language
#' @param ph The phonemes of interest
#' @param lg The language of interest
#' @return The minimal matrix of features given ph and lg
#' @examples
#' getFeat(ph = c("i", "u"), lg = "english");
#' @export

getFeat = function(ph = c(), lg = "Portuguese"){

  require(tidyverse)
  require(gtools)

  load("data/allFeatures.rda")

  phonemes = "i.y.ɨ.ʉ.ɯ.u.ɪ.ʏ.ʊ.e.ø.ɘ.ɵ.ɤ.o.ɛ.œ.ə.ɜ.ɞ.ʌ.ɔ.ɐ.æ.ɶ.a.ɑ.ɒ.ɛ̃.œ̃.ɔ.j.ɥ.w.p.b.t.d.ʈ.ɖ.c.ɟ.k.ɡ.q.ɢ.ʔ.m.ɱ.n.ɳ.ɲ.ŋ.ɴ.ʙ.r.ʀ.ⱱ.ɾ.ɽ.ɸ.β.f.v.θ.ð.s.z.ʃ.ʒ.ʂ.ʐ.ç.ʝ.x.ɣ.χ.ʁ.ħ.ʕ.h.ɦ.ɬ.ɮ.ʋ.ɹ.ɻ.j.ɰ.l.ɭ.ʎ.ʟ.d͡z.t͡s.t͡ʃ.d͡ʒ"

  phonemes = phonemes %>%
    str_split(pattern = "\\.") %>%
    unlist()

  vowels = phonemes[1:31]
  semivowels = phonemes[31:33]
  liquids = c("l.r.ɾ.ɽ.l.ɭ.ʎ.ʟ.ɹ.ɻ.ʁ.ʀ") %>% str_split(pattern = "\\.") %>% unlist()
  nasals = "m.ɱ.n.ɳ.ɲ.ŋ.ɴ" %>% str_split(pattern = "\\.") %>% unlist()
  fricatives = "ɸ.β.f.v.θ.ð.s.z.ʃ.ʒ.ʂ.ʐ.ç.ʝ.x.ɣ.χ.ʁ.ħ.ʕ.h.ɦ" %>% str_split(pattern = "\\.") %>% unlist()
  affricates = "d͡z.t͡s.t͡ʃ.d͡ʒ" %>% str_split(pattern = "\\.") %>% unlist()

  allFeatures = allFeatures %>%
    filter(ipa %in% phonemes) %>%
    droplevels() %>%
    mutate(approx = ifelse(ipa %in% c(vowels, semivowels, liquids), "+", "-")) %>%
    select(ipa, syl, cons, son, approx, cont:hireg)

  # Pick one language to work with:
  portuguese = "a.e.i.o.u.ɛ.ɔ.j.w.p.b.t.d.k.ɡ.f.v.s.z.ʃ.ʒ.m.n.ɲ.l.r.ɾ.ʎ" %>%
    str_split(pattern = "\\.") %>%
    unlist()

  french = c("a.e.ø.ɑ.i.y.o.u.ɛ.ɔ.ə.œ.ɛ̃.œ̃.ɔ̃.ɑ̃.p.b.t.d.k.g.f.v.s.z.ʃ.ʒ.ʁ.m.ɱ.n.ɲ.ŋ.l.w.j.ɥ") %>%
    str_split(pattern = "\\.") %>%
    unlist()

  english = c("a.e.ɑ.i.o.u.ɛ.ɔ.ə.ɪ.ʊ.æ.ʌ.p.b.f.v.t.d.s.z.ʃ.ʒ.t͡ʃ.d͡ʒ.θ.ð.m.n.ŋ.h.w.j.ɹ.l") %>%
    str_split(pattern = "\\.") %>%
    unlist()

  # Select language:
  targetLanguage = eval(parse(text = str_to_lower(lg)))

  # Select features for phonemes:
  targetF = allFeatures %>%
    filter(ipa %in% targetLanguage) %>%
    droplevels()


  # Reduce number of features (remove all that are useless):
  targetF = targetF %>%
    select(where(~n_distinct(.) > 1))

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
    filter(ipa %in% chosenPh) %>%
    droplevels()

  glimpse(chosenPhF)

  # Pick intersection:

  all_cols = names(chosenPhF)[-1]

  uniqueF = chosenPhF %>%
    select(-ipa) %>%
    summarize(across(.cols = all_of(all_cols), .fns = same)) %>%
    select(where(~sum(!is.na(.x)) > 0))

  combinedFeatures = targetF %>% right_join(uniqueF)

  if(nrow(combinedFeatures) > length(chosenPh)){
    return("Not a natural class in this language.")
  } else {


    totalLength = ncol(uniqueF)

    minimalMatrix = FALSE

    for(i in 1:totalLength){

      x1 = combinations(totalLength, i)

      x2 = as.data.frame(x1) %>%
        as_tibble()

      for(j in seq_along(1:nrow(x2))){

        x3 = x2 %>%
          slice(j) %>%
          unlist()

        x4 = uniqueF %>% select(all_of(as.vector(x3)))
        x5 = targetF %>% right_join(x4)

        if(nrow(x5) == nrow(chosenPhF)){
          print("Found minimal matrix:")
          minimalMatrix = TRUE
          return(x4)

          break
        }

      }

      if(isTRUE(minimalMatrix)){
        break
      }
    }
  }
}
