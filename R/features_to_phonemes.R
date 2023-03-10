#' Phoneme generator from distinctive features
#'
#' Returns a set of phonemes given different distinctive feature values.
#' @param ft The abbreviated features of interest: syl, son, cons, cont, DR, lat, nas, strid, vce, sg, cg, ant, cor, distr, lab, hi, lo, back, round, vel, tense, long, hitone, hireg
#' @param lg The language of interest: English, French, Italian, Portuguese, Spanish
#' @return The phonemes given the features provided
#' @examples
#' getPhon(ft = c("+hi", "+tense"), lg = "english");
#' @importFrom magrittr %>%
#' @export

getPhon = function(ft = c(), lg = "Portuguese"){

  features = "syl|son|cons|cont|DR|lat|nas|strid|vce|sg|cg|ant|cor|distr|lab|hi|lo|back|round|vel|tense|long|hitone|hireg"

  plusFt = stringr::str_split(features, pattern = "\\|") %>% unlist() %>% stringr::str_c("+", .)
  minusFt = stringr::str_split(features, pattern = "\\|") %>% unlist() %>% stringr::str_c("-", .)
  zeroFt = stringr::str_split(features, pattern = "\\|") %>% unlist() %>% stringr::str_c("0", .)

  ftCombs = c(plusFt, minusFt, zeroFt)

  checkFt = ft %in% ftCombs

  if(sum(checkFt) != length(checkFt)){
    stop("Incorrect feature. Type ?getPhon to see which features are allowed. All features must be immediately preceded by +, -, or 0.")
  }

  availableLg = c("portuguese", "french", "english", "italian", "spanish")

  if(!stringr::str_to_lower(lg) %in% availableLg){
    stop("Language not supported (or misspelled).")
  }

  # Pick one language to work with:
  portuguese = "a.e.i.o.u.\u025b.\u0254.j.w.p.b.t.d.k.g.f.v.s.z.\u0283.\u0292.m.n.\u0272.l.r.\u027e.\u028e" %>%
    stringr::str_split(pattern = "\\.") %>%
    unlist()

  french = c("a.e.\u00f8.\u0251.i.y.o.u.\u025b.\u0254.\u0259.\u0153.\u025b\u0303.\u0153\u0303.\u0254\u0303.\u0251\u0303.p.b.t.d.k.g.f.v.s.z.\u0283.\u0292.\u0281.m.\u0271.n.\u0272.\u014b.l.w.j.\u0265") %>%
    stringr::str_split(pattern = "\\.") %>%
    unlist()

  english = c("a.e.\u0251.i.o.u.\u025b.\u0254.\u0259.\u026a.\u028a.\u00e6.\u028c.p.b.f.k.g.v.t.d.s.z.\u0283.\u0292.t\u0361\u0283.d\u0361\u0292.\u03b8.\u00f0.m.n.\u014b.h.w.j.\u0279.l") %>%
    stringr::str_split(pattern = "\\.") %>%
    unlist()

  italian = "a.e.i.o.u.\u025b.\u0254.j.w.p.b.t.d.k.g.t\u0361\u0283.d\u0361\u0292.t\u0361s.d\u0361z.f.v.s.z.\u0283.m.n.\u0272.l.r.\u028e" %>%
    stringr::str_split(pattern = "\\.") %>%
    unlist()

  spanish = c("a.e.\u0251.i.o.u.p.b.f.v.t.d.k.g.s.z.t\u0361\u0283.\u03b8.m.\u0272.w.j.l.r.\u027e.\u028e.x.\u029d") %>%
    stringr::str_split(pattern = "\\.") %>%
    unlist()

  # Select language:
  targetLanguage = eval(parse(text = stringr::str_to_lower(lg)))

  allFeatures = allFeatures %>%
    dplyr::filter(ipa %in% targetLanguage) %>%
    droplevels()

  fNames = "syl|son|cons|cont|DR|lat|nas|strid|vce|sg|cg|ant|cor|distr|lab|hi|lo|back|round|vel|tense|long|hitone|hireg"

  # Extract features:
  features = stringr::str_extract_all(string = ft, pattern = fNames) %>% unlist()

  # Extract values:
  values = stringr::str_extract_all(string = ft, pattern = "^[\\+\\-0]") %>% unlist()

  featuresIn = tibble::tibble(feature = features,
                              values = values) %>%
    tidyr::pivot_wider(names_from = feature,
                       values_from = values)

  # phonemes = allFeatures %>%
  #   right_join(featuresIn)
  phonemes = merge(allFeatures, featuresIn, all.y = TRUE)

  if(is.na(phonemes$ipa[1])){
    return("No phonemes with the features in question.")
  }

  # Result:

  return(phonemes$ipa)
}
