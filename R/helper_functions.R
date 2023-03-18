#' Word generator for Portuguese
#'
#' Returns IPA phonemic transcription for a nonce word given a specific weight profile
#' @param profile The weight profile of the desired string using Ls or Hs
#' @param palatalization Whether t and d should palatalize before i (default is FALSE)
#' @noRd
#' @return The IPA transcription of said string
#' @importFrom magrittr %>%

gen_pt = function(profile = "LLL", palatalization = F){

  vowels = "a.e.i.o.u" %>%
    stringr::str_split(pattern = "\\.") %>%
    unlist()
  semivowels = "j.w" %>%
    stringr::str_split(pattern = "\\.") %>%
    unlist()
  liquids = "l.x.\u027e.\u028e" %>%
    stringr::str_split(pattern = "\\.") %>%
    unlist()
  nasals = "m.n.\u0272" %>%
    stringr::str_split(pattern = "\\.") %>%
    unlist()
  fricatives = "f.v.s.z.\u0283.\u0292.x" %>%
    stringr::str_split(pattern = "\\.") %>%
    unlist()
  plosives = "p.b.t.d.k.g" %>%
    stringr::str_split(pattern = "\\.") %>%
    unlist()


  # Desired profile:
  weight = profile # For stress candidates later on
  profile = profile %>%
    stringr::str_split("") %>% unlist()

  # Phonotactics:
  nucleus = vowels
  onsets = "p.b.t.d.k.g.f.v.s.z.\u0283.\u0292.x.m.n.l.x" %>% stringr::str_split(pattern = "\\.") %>% unlist()
  codas = "s.l.\u027e.m.n" %>% stringr::str_split(pattern = "\\.") %>% unlist()

  clusters = "p\u027e.b\u027e.t\u027e.d\u027e.k\u027e.g\u027e.pl.bl.kl.gl"

  # Syllables:
  syllables = c()


  # Create syllables and avoid two identical onsets in sequence (OCP):
  for(s in 1:length(profile)){
    if(profile[s] == "L"){
      # create light syllable

      if(s == 1){

        syllables[length(syllables)+1] = stringr::str_c(sample(onsets, size = 1),
                                               sample(nucleus, size = 1),
                                               collapse = "")
      }

      else {
        candidate = stringr::str_c(sample(onsets, size = 1),
                          sample(nucleus, size = 1),
                          collapse = "")
        previousOnset = stringr::str_extract(string = syllables[length(syllables)], pattern = "^\\w{1}")
        currentOnset = stringr::str_extract(string = candidate, pattern = "^\\w{1}")

        while(previousOnset == currentOnset){
          candidate = stringr::str_c(sample(onsets, size = 1),
                            sample(nucleus, size = 1),
                            collapse = "")
          previousOnset = stringr::str_extract(string = syllables[length(syllables)], pattern = "^\\w{1}")
          currentOnset = stringr::str_extract(string = candidate, pattern = "^\\w{1}")
        }

        while(previousOnset %in% c("s", "z", "\u0283", "\u0292") & currentOnset %in% c("s", "z", "\u0283", "\u0292")){
          candidate = stringr::str_c(sample(onsets, size = 1),
                            sample(nucleus, size = 1),
                            collapse = "")
          previousOnset = stringr::str_extract(string = syllables[length(syllables)], pattern = "^\\w{1}")
          currentOnset = stringr::str_extract(string = candidate, pattern = "^\\w{1}")
        }

        while(previousOnset %in% c("m", "n") & currentOnset %in% c("m", "n")){
          candidate = stringr::str_c(sample(onsets, size = 1),
                            sample(nucleus, size = 1),
                            collapse = "")
          previousOnset = stringr::str_extract(string = syllables[length(syllables)], pattern = "^\\w{1}")
          currentOnset = stringr::str_extract(string = candidate, pattern = "^\\w{1}")
        }

        syllables[length(syllables)+1] = candidate

      }

    } else if(profile[s] == "H"){
      # create heavy syllable

      if(s == 1){

        syllables[length(syllables)+1] = stringr::str_c(sample(onsets, size = 1),
                                               sample(nucleus, size = 1),
                                               sample(codas, size = 1),
                                               collapse = "")
      }

      else {
        candidate = stringr::str_c(sample(onsets, size = 1),
                          sample(nucleus, size = 1),
                          sample(codas, size = 1),
                          collapse = "")
        previousOnset = stringr::str_extract(string = syllables[length(syllables)], pattern = "^\\w{1}")
        currentOnset = stringr::str_extract(string = candidate, pattern = "^\\w{1}")

        while(previousOnset == currentOnset){
          candidate = stringr::str_c(sample(onsets, size = 1),
                            sample(nucleus, size = 1),
                            sample(codas, size = 1),
                            collapse = "")
          previousOnset = stringr::str_extract(string = syllables[length(syllables)], pattern = "^\\w{1}")
          currentOnset = stringr::str_extract(string = candidate, pattern = "^\\w{1}")
        }

        while(previousOnset %in% c("s", "z", "\u0283", "\u0292") & currentOnset %in% c("s", "z", "\u0283", "\u0292")){
          candidate = stringr::str_c(sample(onsets, size = 1),
                            sample(nucleus, size = 1),
                            sample(codas, size = 1),
                            collapse = "")
          previousOnset = stringr::str_extract(string = syllables[length(syllables)], pattern = "^\\w{1}")
          currentOnset = stringr::str_extract(string = candidate, pattern = "^\\w{1}")
        }

        while(previousOnset %in% c("m", "n") & currentOnset %in% c("m", "n")){
          candidate = stringr::str_c(sample(onsets, size = 1),
                            sample(nucleus, size = 1),
                            sample(codas, size = 1),
                            collapse = "")
          previousOnset = stringr::str_extract(string = syllables[length(syllables)], pattern = "^\\w{1}")
          currentOnset = stringr::str_extract(string = candidate, pattern = "^\\w{1}")
        }

        syllables[length(syllables)+1] = candidate

      }
    }
  }



  # Replace i/u final with e/o:
  syllables

  if(stringr::str_detect(string = syllables[length(syllables)],
                pattern = "[iu]\\w*$")){

    syllables[length(syllables)] = stringr::str_replace(string = syllables[length(syllables)],
                                               pattern = "u$",
                                               replacement = "o")
    syllables[length(syllables)] = stringr::str_replace(string = syllables[length(syllables)],
                                               pattern = "i$",
                                               replacement = "e")
  }

  # Now add clusters and low-mid vowels:

  # Clusters: at most 1/word
  nSyl = syllables %>% length()

  # Create a version where all candidates have complex onsets:
  clusterSyl = syllables %>%
    stringr::str_replace(pattern = "([pbtdkgfv])([aeiou])",
                replacement = "\\1\u027e\\2")

  randomOrder = sample(x = seq(1, nSyl), size = nSyl, replace = F)

  # Now loop through syllables to add one cluster:
  for(i in randomOrder){
    if(stringr::str_detect(string = syllables[i],
                  pattern = "([pbtdkgfv])([aeiou])")){
      syllables[i] = clusterSyl[i]
      break
    }
  }

  # Now we have (at most) on syllable with a complex onset in "syllables"
  word = syllables %>%
    stringr::str_c(collapse = "")

  # double_C = function(s = ""){
  #
  #   doubleCs = "p{2,}|b{2,}|c{2,}|t{2,}|d{2,}|k{2,}|g{2,}|l{2,}|m{2,}|n{2,}|f{2,}|v{2,}"
  #
  #   single_C = stringr::str_extract(s,
  #                          pattern = doubleCs) %>%
  #     stringr::str_sub(start = 1, end = 1)
  #
  #   empty_s = stringr::str_replace_all(s,
  #                             pattern = doubleCs,
  #                             replacement = "#")
  #
  #   final_s = empty_s %>%
  #     stringr::str_replace_all(pattern = "#",
  #                     replacement = single_C)
  #
  #   return(final_s)
  #
  #
  # }


  # assign stress probabilistically:
  word = word %>%
    # double_C() %>%
    syllabify_pt()

  if(weight %in% c("HLL", "LLL")){
    word = word %>%
      apu_candidates() %>%
      dact_pt()
  }

  else if(weight %in% c("LLH", "LH", "HH", "LHH")){
    word = word %>%
      pu_candidates() %>%
      spond_pt()
  } else {
    word = word %>%
      stress_pt()
  }


  # Now add low-mid if the syllable is stressed /e o/
  toLower = sample(x = c(0,1), size = 1, prob = c(0.6, 0.4))

  if(toLower == 1){
    word = stringr::str_replace(word,
                       pattern = "(\u02c8\\w{1,2})[e]",
                       replacement = "\\1\u025b")

    word = stringr::str_replace(word,
                       pattern = "(\u02c8\\w{1,2})[o]",
                       replacement = "\\1\u0254")

  }

  # Final corrections given phonotactic rules in Portuguese:

  if(palatalization == T){

  word = stringr::str_replace_all(word,
                         pattern = "t([i])",
                         replacement = "t\u0361\u0283\\1")

  word = stringr::str_replace_all(word,
                         pattern = "d([i])",
                         replacement = "d\u0361\u0292\\1")

  }

  word = stringr::str_replace_all(word,
                         pattern = "l($|\\.)",
                         replacement = "w\\1")

  word = stringr::str_replace_all(word,
                         pattern = "n$",
                         replacement = "m")

  word = stringr::str_replace_all(word,
                         pattern = "s\\.\u02c8?[\u0292\u0283]",
                         replacement = "s.t")

  word = stringr::str_replace_all(word,
                         pattern = "\u0254([mn])",
                         replacement = "o\\1")

  word = stringr::str_replace_all(word,
                         pattern = "\u025b([mn])",
                         replacement = "e\\1")

  word = stringr::str_replace_all(word,
                         pattern = "m\\.(\u02c8?[fvsztdkg\u0292\u0283x])",
                         replacement = "n.\\1")

  word = stringr::str_replace_all(word,
                         pattern = "n\\.(\u02c8?[pb])",
                         replacement = "m.\\1")

  word = stringr::str_replace_all(word,
                         pattern = "uw",
                         replacement = "ow")



  # OCP for vowels:
  word = stringr::str_replace(string = word,
                     pattern = "(\\w+a\\w*\\.\\w+)a(\\w*\\.\\w+a\\w*)",
                     replacement = "\\1i\\2")

  word = stringr::str_replace(string = word,
                     pattern = "(\\w+e\\w*\\.\\w+)e(\\w*\\.\\w+e\\w*)",
                     replacement = "\\1u\\2")

  word = stringr::str_replace(string = word,
                     pattern = "(\\w+o\\w*\\.\\w+)o(\\w*\\.\\w+o\\w*)",
                     replacement = "\\1a\\2")

  word = stringr::str_replace(string = word,
                     pattern = "(\\w+i\\w*\\.\\w+)i(\\w*\\.\\w+i\\w*)",
                     replacement = "\\1e\\2")

  word = stringr::str_replace(string = word,
                     pattern = "(\\w+u\\w*\\.\\w+)u(\\w*\\.\\w+u\\w*)",
                     replacement = "\\1o\\2")

  word = stringr::str_replace_all(word,
                         pattern = "(s\\.\u02c8?)s",
                         replacement = "\\1t")


  # OCP for coda-onsets:
  # "s.l.\u027e.m.n"

  word = stringr::str_replace_all(word,
                         pattern = "(n\\.\u02c8?)m",
                         replacement = "\\1t")

  word = stringr::str_replace_all(word,
                         pattern = "(m\\.\u02c8?)n",
                         replacement = "\\1p")

  word = stringr::str_replace_all(word,
                         pattern = "(n\\.\u02c8?)n",
                         replacement = "\\1d")

  word = stringr::str_replace_all(word,
                         pattern = "(m\\.\u02c8?)m",
                         replacement = "\\1p")

  word = stringr::str_replace_all(word,
                         pattern = "(s\\.\u02c8?)s",
                         replacement = "\\1t")

  word = stringr::str_replace_all(word,
                         pattern = "(l\\.\u02c8?)l",
                         replacement = "\\1t")

  word = stringr::str_replace_all(word,
                         pattern = "(\u027e\\.\u02c8?)\u027e",
                         replacement = "\\1d")

  return(word)
}

#' Bigram probability for Portuguese - helper function
#'
#' Given a phonemically transcribed string, the function returns its bigram probability in log using the lexicon in the Portuguese Stress Lexicon as reference
#'
#' @param word A possible string in Portuguese in its phonemic form without syllabification or stress. The only diacritic that should be used is the tilde for nasals, e.g., ã.
#' @return The phonemic transcription for the string in question
#' @noRd
#' @importFrom magrittr %>%

biGram_pt_helper = function(word = ""){

  if(stringr::str_detect(string = word, pattern = "[chqyw]")){
    message("Input most be phonemic, not orthographic.")
    return(NA)
  }
  word = word %>%
    stringr::str_remove_all("\\.|\u02c8") %>%
    stringr::str_c("^", ., "$") %>%
    stringr::str_split("") %>%
    unlist() %>%
    stringr::str_c(collapse = " ")

  bigramProb = ngram::ngram(str = word, n = 2) %>%
    ngram::get.phrasetable() %>%
    tibble::as_tibble() %>%
    tidyr::uncount(freq) %>%
    dplyr::mutate(ngrams = str_remove_all(ngrams, pattern = "\\s")) %>%
    dplyr::select(-c(prop)) %>%
    dplyr::left_join(bigrams_pt, by = "ngrams") %>%
    dplyr::filter(!ngrams %in% c("^^", "$$", "^$", "$^"))

  bigramProb[is.na(bigramProb$prop),]$prop = 1e-10

  bigramProb %>%
    dplyr::summarize(prob = log(prod(prop))) %>%
    dplyr::pull(prob) %>%
    return()

}

