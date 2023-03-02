#' Sonority dispersion calculator
#'
#' Given a demisyllable, the function returns its sonority dispersion score based on the
#' sonority sequencing principle
#' @param demi A demi syllable; see function demi()
#' @return The sonority dispersion score
#' @examples
#' sonDisp(demi = "tra");
#' @importFrom magrittr %>%
#' @export

sonDisp = function(demi = ""){

  # Define classes by sonority:
  vowels = "aeiouɛɔøɑəɪʊæœɛ̃œ̃ɔ̃" %>%
    stringr::str_split("") %>% unlist()

  glides = "jwʍɥ" %>%
    stringr::str_split("") %>% unlist()

  liquids = "lrɾɹʁʎ" %>%
    stringr::str_split("") %>% unlist()

  nasals = "mnŋɲ" %>%
    stringr::str_split("") %>% unlist()

  obs = "p.b.t.d.k.g.f.v.s.z.θ.ð.ʃ.ʝ.ʒ.t͡ʃ.d͡ʒ.h" %>%
    stringr::str_split("\\.") %>% unlist()


  # Create tibble with all classes:
  sonority = tibble::tibble(phoneme = c(vowels, glides, liquids, nasals, obs),
                            class = c(rep("vowel", times = length(vowels)),
                                      rep("glide", times = length(glides)),
                                      rep("liquid", times = length(liquids)),
                                      rep("nasal", times = length(nasals)),
                                      rep("obstruent", times = length(obs))),
                            score = dplyr::case_when(class == "vowel" ~ 5,
                                                     class == "glide" ~ 4,
                                                     class == "liquid" ~ 3,
                                                     class == "nasal" ~ 2,
                                                     class == "obstruent" ~ 1))

  d = demi %>% stringr::str_split("") %>% unlist()
  d2 = tibble::tibble(phoneme = d) %>%
    dplyr::left_join(sonority, by = "phoneme")
  score = d2 %>% dplyr::pull(score)
  combs = combn(score, 2)
  diffs = combs[1,] - combs[2,]
  D = sum(1/diffs^2)

  if(is.infinite(D)){
    message("This is a plateau.")
    return(NA)

  }
  return(round(D, 2))

}
