#' Sonority dispersion calculator
#'
#' Given a demisyllable, the function returns its sonority dispersion score based on the
#' sonority sequencing principle
#' @param demi A demi syllable; see function \code{demi()}
#' @return The sonority dispersion score
#' @examples
#' sonDisp(demi = "tra");
#' @export

sonDisp = function(demi = ""){

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

  d = demi |> stringr::str_split("") |> unlist()
  d2 = tibble::tibble(phoneme = d) |>
    dplyr::left_join(sonority, by = "phoneme")
  score = d2 |> dplyr::pull(score)
  combs = combn(score, 2)
  diffs = combs[1,] - combs[2,]
  D = sum(1/diffs^2)

  if(is.infinite(D)){
    message("This is a plateau.")
    return(NA)

  }
  return(round(D, 2))

}
