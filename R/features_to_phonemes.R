#' Phoneme generator from distinctive features
#'
#' Returns a set of phonemes given different distinctive feature values.
#' @param ft The abbreviated features of interest: syl, son, cons, cont, DR, lat, nas, strid, vce, sg, cg, ant, cor, distr, lab, hi, lo, back, round, vel, tense, long, hitone, hireg
#' @param lg The language of interest
#' @return The phonemes given the features provided
#' @examples
#' getFeat(ft = c("+hi", "+tense"), lg = "english");
#' @export

getPhon = function(ft = c(), lg = "Portuguese"){

  require(tidyverse, quietly = TRUE)
  require(gtools, quietly = TRUE)

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

  allFeatures = allFeatures %>%
    filter(ipa %in% targetLanguage) %>%
    droplevels()

  fNames = "syl|son|cons|cont|DR|lat|nas|strid|vce|sg|cg|ant|cor|distr|lab|hi|lo|back|round|vel|tense|long|hitone|hireg"

  # Extract features:
  features = str_extract_all(string = ft, pattern = fNames) %>% unlist()

  # Extract values:
  values = str_extract_all(string = ft, pattern = "^[\\+\\-0]") %>% unlist()

  featuresIn = tibble(feature = features,
                      values = values) %>%
    pivot_wider(names_from = feature,
                values_from = values)

  phonemes = allFeatures %>%
    right_join(featuresIn)

  if(is.na(phonemes$ipa[1])){
    return("No phonemes with the features in question.")
  }

  # Result:

  return(phonemes$ipa)
}
