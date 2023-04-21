#' Distinctive feature generator
#'
#' Generates a feature matrix for a given set of phonemes in a given language
#' @param ph The phonemes of interest
#' @param lg The language of interest: English, French, Italian, Portuguese, Spanish
#' @return The minimal matrix of features given \code{ph} and \code{lg}
#' @examples
#' getFeat(ph = c("i", "u"), lg = "english");
#' @export

getFeat = function(ph = c(), lg = "Portuguese"){

  phonemes = "i.y.\u0268.\u0289.\u026f.u.\u026a.\u028f.\u028a.e.\u00f8.\u0258.\u0275.\u0264.o.\u025b.\u0153.\u0259.\u025c.\u025e.\u028c.\u0254.\u0250.\u00e6.\u0276.a.\u0251.\u0252.\u025b\u0303.\u0153\u0303.\u0254.j.\u0265.w.p.b.t.d.\u0288.\u0256.c.\u025f.k.\u0261.q.\u0262.\u0294.m.n.\u0273.\u0272.\u014b.\u0274.\u0299.r.\u0280.\u2c71.\u027e.\u027d.\u0278.\u03b2.f.v.\u03b8.\u00f0.s.z.\u0283.\u0292.\u0282.\u0290.\u00e7.\u029d.x.\u0263.\u03c7.\u0281.\u0127.\u0295.h.\u0266.\u026c.\u026e.\u028b.\u0279.\u027b.j.\u0270.l.\u026d.\u028e.\u029f.d\u0361z.t\u0361s.t\u0361\u0283.d\u0361\u0292"

  phonemes = phonemes |>
    stringr::str_split(pattern = "\\.") |>
    unlist()

  # vowels = phonemes[1:31]
  # semivowels = phonemes[31:33]
  # liquids = c("l.r.\u027e.\u027d.l.\u026d.\u028e.\u029f.\u0279.\u027b.\u0281.\u0280") |> stringr::str_split(pattern = "\\.") |> unlist()
  # nasals = "m.\u0271.n.\u0273.\u0272.\u014b.\u0274" |> stringr::str_split(pattern = "\\.") |> unlist()
  # fricatives = "\u0278.\u03b2.f.v.\u03b8.\u00f0.s.z.\u0283.\u0292.\u0282.\u0290.\u00e7.\u029d.x.\u0263.\u03c7.\u0281.\u0127.\u0295.h.\u0266" |> stringr::str_split(pattern = "\\.") |> unlist()
  # affricates = "d\u0361z.t\u0361s.t\u0361\u0283.d\u0361\u0292" |> stringr::str_split(pattern = "\\.") |> unlist()

  allFeatures = allFeatures |>
    dplyr::filter(ipa %in% phonemes) |>
    droplevels() |>
    # dplyr::mutate(approx = ifelse(ipa %in% c(vowels, semivowels, liquids), "+", "-")) |>
    dplyr::select(ipa, syl, cons, son, cont:approx)

  # Pick one language to work with:
  portuguese = "a.e.i.o.u.\u025b.\u0254.j.w.p.b.t.d.k.g.f.v.s.z.\u0283.\u0292.m.n.\u0272.l.r.\u027e.\u028e" |>
    stringr::str_split(pattern = "\\.") |>
    unlist()

  french = c("a.e.\u00f8.\u0251.i.y.o.u.\u025b.\u0254.\u0259.\u0153.\u025b\u0303.\u0153\u0303.\u0254\u0303.\u0251\u0303.p.b.t.d.k.g.f.v.s.z.\u0283.\u0292.\u0281.m.\u0271.n.\u0272.\u014b.l.w.j.\u0265") |>
    stringr::str_split(pattern = "\\.") |>
    unlist()

  english = c("a.e.\u0251.i.o.u.\u025b.\u0254.\u0259.\u026a.\u028a.\u00e6.\u028c.p.b.f.k.g.v.t.d.s.z.\u0283.\u0292.t\u0361\u0283.d\u0361\u0292.\u03b8.\u00f0.m.n.\u014b.h.w.j.\u0279.l") |>
    stringr::str_split(pattern = "\\.") |>
    unlist()

  italian = "a.e.i.o.u.\u025b.\u0254.j.w.p.b.t.d.k.g.t\u0361\u0283.d\u0361\u0292.t\u0361s.d\u0361z.f.v.s.z.\u0283.m.n.\u0272.l.r.\u028e" |>
    stringr::str_split(pattern = "\\.") |>
    unlist()

  spanish = c("a.e.i.o.u.p.b.f.v.t.d.k.g.s.z.t\u0361\u0283.\u03b8.m.\u0272.w.j.l.r.\u027e.\u028e.x.\u029d") |>
    stringr::str_split(pattern = "\\.") |>
    unlist()

  pt = portuguese
  fr = french
  en = english
  it = italian
  sp = spanish

  availableLg = c("portuguese", "pt", "french", "fr", "italian", "it", "english", "en", "spanish", "sp")

  if(!stringr::str_to_lower(lg) %in% availableLg){
    stop("Language not supported (or misspelled).")
  }

  # Select language:
  targetLanguage = eval(parse(text = stringr::str_to_lower(lg)))

  # Select features for phonemes:
  targetF = allFeatures |>
    dplyr::filter(ipa %in% targetLanguage) |>
    droplevels()


  # Reduce number of features (remove all that are useless):
  targetF = targetF |>
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
    if(chosenPh[phoneme] == "t\u0283"){
      chosenPh[phoneme] = "t\u0361\u0283"
    } else if(chosenPh[phoneme] == "d\u0292"){
      chosenPh[phoneme] = "d\u0361\u0292"
    } else {
      chosenPh[phoneme] = chosenPh[phoneme]
    }
  }

  if(!all(chosenPh %in% targetLanguage)){
    stop("Input doesn\'t match phonemic inventory in language.")
  }

  chosenPhF = targetF |>
    dplyr::filter(ipa %in% chosenPh) |>
    droplevels()

  # Pick intersection:

  all_cols = names(chosenPhF)[-1]

  uniqueF = chosenPhF |>
    dplyr::select(-ipa) |>
    dplyr::summarize(dplyr::across(.cols = dplyr::all_of(all_cols), .fns = same)) |>
    dplyr::select(dplyr::where(~sum(!is.na(.x)) > 0))

  # combinedFeatures = targetF |> right_join(uniqueF)

  combinedFeatures = merge(targetF, uniqueF, all.y = TRUE)

  if(nrow(combinedFeatures) > length(chosenPh)){
    return("Not a natural class in this language.")
  } else {


    totalLength = ncol(uniqueF)

    minimalMatrix = FALSE

    for(i in 1:totalLength){

      x1 = gtools::combinations(totalLength, i)

      x2 = as.data.frame(x1) |>
        dplyr::as_tibble(.name_repair = "minimal")

      for(j in seq_along(1:nrow(x2))){

        x3 = x2 |>
          dplyr::slice(j) |>
          unlist()

        x4 = uniqueF |> dplyr::select(dplyr::all_of(as.vector(x3)))
        # x5 = targetF |> right_join(x4)
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
