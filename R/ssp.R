#' Sonority sequencing principle
#'
#' Given a demisyllable, the function returns 1 if the demisyllable respects
#' the sonority sequencing principle (SSP) and 0 otherwise
#' @param demi A demi syllable; see function demi()
#' @param d Whether the function should return first (1) or second (2) demisyllable
#' @return Either 1, indicating that the sequence respects the SSP, or 0
#' @examples
#' ssp(demi = "tra", d = 1);
#' @export

ssp = function(demi = "", d = 1){
  if(!require("pacman", quietly = T)){install.packages("pacman")}
  pacman::p_load(tidyverse)

  # Define classes by sonority:
  vowels = "aeiouɛɔøɑəɪʊæœɛ̃œ̃ɔ̃" %>%
    str_split("") %>% unlist()

  glides = "jwʍɥ" %>%
    str_split("") %>% unlist()

  liquids = "lrɾɹʁʎ" %>%
    str_split("") %>% unlist()

  nasals = "mnŋɲ" %>%
    str_split("") %>% unlist()

  obs = "p.b.t.d.k.g.f.v.s.z.θ.ð.ʃ.ʝ.ʒ.t͡ʃ.d͡ʒ.h" %>%
    str_split("\\.") %>% unlist()


  # Create tibble with all classes:
  sonority = tibble(phoneme = c(vowels, glides, liquids, nasals, obs),
                    class = c(rep("vowel", times = length(vowels)),
                              rep("glide", times = length(glides)),
                              rep("liquid", times = length(liquids)),
                              rep("nasal", times = length(nasals)),
                              rep("obstruent", times = length(obs))),
                    score = case_when(class == "vowel" ~ 5,
                                      class == "glide" ~ 4,
                                      class == "liquid" ~ 3,
                                      class == "nasal" ~ 2,
                                      class == "obstruent" ~ 1))

  if(d == 1){
    demi = demi(word = demi, d = 1)
  } else if(d == 2){
    demi = demi(word = demi, d = 2)
  } else {
    message("d must be 1 or 2")
    return(NA)
  }

  demi_split = demi %>%
    str_split("") %>%
    unlist()

  tib = tibble(phoneme = demi_split) %>%
    left_join(sonority, by = "phoneme") %>%
    pull(score)

  if(any(is.na(tib))){
    message("One of the phonemes you entered is not valid.")
    return(NA)
  }

  rising = all(sort(tib, decreasing = F) == tib)
  falling = all(sort(tib, decreasing = T) == tib)
  noPlateau = !any(duplicated(tib))

  if((d == 1 & rising & noPlateau) | d == 2 & falling & noPlateau){# must be ascending
    return(1)

  } else {
    return(0)
  }

}
