#' Sonority dispersion calculator
#'
#' Given a demisyllable, the function returns its sonority dispersion score based on the
#' sonority sequencing principle
#' @param demi A demi syllable; see function demi()
#' @return The sonority dispersion score
#' @examples
#' sonDisp(demi = "tra");
#' @export

sonDisp = function(demi = ""){
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

  d = demi %>% str_split("") %>% unlist()
  d2 = tibble(phoneme = d) %>%
    left_join(sonority, by = "phoneme")
  score = d2 %>% pull(score)
  combs = combn(score, 2)
  diffs = combs[1,] - combs[2,]
  D = sum(1/diffs^2)

  if(is.infinite(D)){
    message("This is a plateau.")
    return(NA)

  }
  return(round(D, 2))

}
