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

  # Define classes by sonority:
  vowels = "aeiou\u025b\u0254\u00f8\u0251\u0259\u026a\u028a\u00e6\u0153\u025b\u0303\u0153\u0303\u0254\u0303" |>
    stringr::str_split("") |> unlist()

  glides = "jw\u028d\u0265" |>
    stringr::str_split("") |> unlist()

  liquids = "lr\u027e\u0279\u0281\u028e" |>
    stringr::str_split("") |> unlist()

  nasals = "mn\u014b\u0272" |>
    stringr::str_split("") |> unlist()

  obs = "p.b.t.d.k.g.f.v.s.z.\u03b8.\u00f0.\u0283.\u029d.\u0292.t\u0361\u0283.d\u0361\u0292.h" |>
    stringr::str_split("\\.") |> unlist()


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

  if(d == 1){
    demi = demi(word = demi, d = 1)
  } else if(d == 2){
    demi = demi(word = demi, d = 2)
  } else {
    message("d must be 1 or 2")
    return(NA)
  }

  demi_split = demi |>
    stringr::str_split("") |>
    unlist()

  tib = tibble::tibble(phoneme = demi_split) |>
    dplyr::left_join(sonority, by = "phoneme") |>
    dplyr::pull(score)

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
