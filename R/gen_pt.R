
#' Word generator for Portuguese
#'
#' Returns IPA phonemic transcription for a nonce word given a specific weight profile
#' @param profile The weight profile of the desired string using Ls or Hs
#' @param palatalization Whether t and d should palatalize before i (default is FALSE)
#' @return The IPA transcription of said string
#' @examples
#' gen_pt(profile = "HLL")
#' @export

gen_pt = function(profile = "LLL", palatalization = F){

  if (!require("pacman", quietly = T)){install.packages("pacman")}
  pacman::p_load(tidyverse)

  vowels = "a.e.i.o.u" %>%
    str_split(pattern = "\\.") %>%
    unlist()
  semivowels = "j.w" %>%
    str_split(pattern = "\\.") %>%
    unlist()
  liquids = "l.x.ɾ.ʎ" %>%
    str_split(pattern = "\\.") %>%
    unlist()
  nasals = "m.n.ɲ" %>%
    str_split(pattern = "\\.") %>%
    unlist()
  fricatives = "f.v.s.z.ʃ.ʒ.x" %>%
    str_split(pattern = "\\.") %>%
    unlist()
  plosives = "p.b.t.d.k.g" %>%
    str_split(pattern = "\\.") %>%
    unlist()


  # Desired profile:
  weight = profile # For stress candidates later on
  profile = profile %>%
    str_split("") %>% unlist()

  # Phonotactics:
  nucleus = vowels
  onsets = "p.b.t.d.k.g.f.v.s.z.ʃ.ʒ.x.m.n.l.x" %>% str_split(pattern = "\\.") %>% unlist()
  codas = "s.l.ɾ.m.n" %>% str_split(pattern = "\\.") %>% unlist()

  clusters = "pɾ.bɾ.tɾ.dɾ.kɾ.gɾ.pl.bl.kl.gl"

  # Syllables:
  syllables = c()


  # Create syllables and avoid two identical onsets in sequence (OCP):
  for(s in 1:length(profile)){
    if(profile[s] == "L"){
      # create light syllable

      if(s == 1){

        syllables[length(syllables)+1] = str_c(sample(onsets, size = 1),
                                               sample(nucleus, size = 1),
                                               collapse = "")
      }

      else {
        candidate = str_c(sample(onsets, size = 1),
                          sample(nucleus, size = 1),
                          collapse = "")
        previousOnset = str_extract(string = syllables[length(syllables)], pattern = "^\\w{1}")
        currentOnset = str_extract(string = candidate, pattern = "^\\w{1}")

        while(previousOnset == currentOnset){
          candidate = str_c(sample(onsets, size = 1),
                            sample(nucleus, size = 1),
                            collapse = "")
          previousOnset = str_extract(string = syllables[length(syllables)], pattern = "^\\w{1}")
          currentOnset = str_extract(string = candidate, pattern = "^\\w{1}")
        }

        while(previousOnset %in% c("s", "z", "ʃ", "ʒ") & currentOnset %in% c("s", "z", "ʃ", "ʒ")){
          candidate = str_c(sample(onsets, size = 1),
                            sample(nucleus, size = 1),
                            collapse = "")
          previousOnset = str_extract(string = syllables[length(syllables)], pattern = "^\\w{1}")
          currentOnset = str_extract(string = candidate, pattern = "^\\w{1}")
        }

        while(previousOnset %in% c("m", "n") & currentOnset %in% c("m", "n")){
          candidate = str_c(sample(onsets, size = 1),
                            sample(nucleus, size = 1),
                            collapse = "")
          previousOnset = str_extract(string = syllables[length(syllables)], pattern = "^\\w{1}")
          currentOnset = str_extract(string = candidate, pattern = "^\\w{1}")
        }

        syllables[length(syllables)+1] = candidate

      }

    } else if(profile[s] == "H"){
      # create heavy syllable

      if(s == 1){

        syllables[length(syllables)+1] = str_c(sample(onsets, size = 1),
                                               sample(nucleus, size = 1),
                                               sample(codas, size = 1),
                                               collapse = "")
      }

      else {
        candidate = str_c(sample(onsets, size = 1),
                          sample(nucleus, size = 1),
                          sample(codas, size = 1),
                          collapse = "")
        previousOnset = str_extract(string = syllables[length(syllables)], pattern = "^\\w{1}")
        currentOnset = str_extract(string = candidate, pattern = "^\\w{1}")

        while(previousOnset == currentOnset){
          candidate = str_c(sample(onsets, size = 1),
                            sample(nucleus, size = 1),
                            sample(codas, size = 1),
                            collapse = "")
          previousOnset = str_extract(string = syllables[length(syllables)], pattern = "^\\w{1}")
          currentOnset = str_extract(string = candidate, pattern = "^\\w{1}")
        }

        while(previousOnset %in% c("s", "z", "ʃ", "ʒ") & currentOnset %in% c("s", "z", "ʃ", "ʒ")){
          candidate = str_c(sample(onsets, size = 1),
                            sample(nucleus, size = 1),
                            sample(codas, size = 1),
                            collapse = "")
          previousOnset = str_extract(string = syllables[length(syllables)], pattern = "^\\w{1}")
          currentOnset = str_extract(string = candidate, pattern = "^\\w{1}")
        }

        while(previousOnset %in% c("m", "n") & currentOnset %in% c("m", "n")){
          candidate = str_c(sample(onsets, size = 1),
                            sample(nucleus, size = 1),
                            sample(codas, size = 1),
                            collapse = "")
          previousOnset = str_extract(string = syllables[length(syllables)], pattern = "^\\w{1}")
          currentOnset = str_extract(string = candidate, pattern = "^\\w{1}")
        }

        syllables[length(syllables)+1] = candidate

      }
    }
  }



  # Replace i/u final with e/o:
  syllables

  if(str_detect(string = syllables[length(syllables)],
                pattern = "[iu]\\w*$")){

    syllables[length(syllables)] = str_replace(string = syllables[length(syllables)],
                                               pattern = "u$",
                                               replacement = "o")
    syllables[length(syllables)] = str_replace(string = syllables[length(syllables)],
                                               pattern = "i$",
                                               replacement = "e")
  }

  # Now add clusters and low-mid vowels:

  # Clusters: at most 1/word
  nSyl = syllables %>% length()

  # Create a version where all candidates have complex onsets:
  clusterSyl = syllables %>%
    str_replace(pattern = "([pbtdkgfv])([aeiou])",
                replacement = "\\1ɾ\\2")

  randomOrder = sample(x = seq(1, nSyl), size = nSyl, replace = F)

  # Now loop through syllables to add one cluster:
  for(i in randomOrder){
    if(str_detect(string = syllables[i],
                  pattern = "([pbtdkgfv])([aeiou])")){
      syllables[i] = clusterSyl[i]
      break
    }
  }

  # Now we have (at most) on syllable with a complex onset in "syllables"
  word = syllables %>%
    str_c(collapse = "")

  # double_C = function(s = ""){
  #
  #   doubleCs = "p{2,}|b{2,}|c{2,}|t{2,}|d{2,}|k{2,}|g{2,}|l{2,}|m{2,}|n{2,}|f{2,}|v{2,}"
  #
  #   single_C = str_extract(s,
  #                          pattern = doubleCs) %>%
  #     str_sub(start = 1, end = 1)
  #
  #   empty_s = str_replace_all(s,
  #                             pattern = doubleCs,
  #                             replacement = "#")
  #
  #   final_s = empty_s %>%
  #     str_replace_all(pattern = "#",
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
    word = str_replace(word,
                       pattern = "(ˈ\\w{1,2})[e]",
                       replacement = "\\1ɛ")

    word = str_replace(word,
                       pattern = "(ˈ\\w{1,2})[o]",
                       replacement = "\\1ɔ")

  }

  # Final corrections given phonotactic rules in Portuguese:

  if(palatalization == T){

  word = str_replace_all(word,
                         pattern = "t([i])",
                         replacement = "t͡ʃ\\1")

  word = str_replace_all(word,
                         pattern = "d([i])",
                         replacement = "d͡ʒ\\1")

  }

  word = str_replace_all(word,
                         pattern = "l($|\\.)",
                         replacement = "w\\1")

  word = str_replace_all(word,
                         pattern = "n$",
                         replacement = "m")

  word = str_replace_all(word,
                         pattern = "s\\.ˈ?[ʒʃ]",
                         replacement = "s.t")

  word = str_replace_all(word,
                         pattern = "ɔ([mn])",
                         replacement = "o\\1")

  word = str_replace_all(word,
                         pattern = "ɛ([mn])",
                         replacement = "e\\1")

  word = str_replace_all(word,
                         pattern = "m\\.(ˈ?[fvsztdkgʒʃx])",
                         replacement = "n.\\1")

  word = str_replace_all(word,
                         pattern = "n\\.(ˈ?[pb])",
                         replacement = "m.\\1")

  word = str_replace_all(word,
                         pattern = "uw",
                         replacement = "ow")



  # OCP for vowels:
  word = str_replace(string = word,
                     pattern = "(\\w+a\\w*\\.\\w+)a(\\w*\\.\\w+a\\w*)",
                     replacement = "\\1i\\2")

  word = str_replace(string = word,
                     pattern = "(\\w+e\\w*\\.\\w+)e(\\w*\\.\\w+e\\w*)",
                     replacement = "\\1u\\2")

  word = str_replace(string = word,
                     pattern = "(\\w+o\\w*\\.\\w+)o(\\w*\\.\\w+o\\w*)",
                     replacement = "\\1a\\2")

  word = str_replace(string = word,
                     pattern = "(\\w+i\\w*\\.\\w+)i(\\w*\\.\\w+i\\w*)",
                     replacement = "\\1e\\2")

  word = str_replace(string = word,
                     pattern = "(\\w+u\\w*\\.\\w+)u(\\w*\\.\\w+u\\w*)",
                     replacement = "\\1o\\2")

  word = str_replace_all(word,
                         pattern = "(s\\.ˈ?)s",
                         replacement = "\\1t")


  # OCP for coda-onsets:
  # "s.l.ɾ.m.n"

  word = str_replace_all(word,
                         pattern = "(n\\.ˈ?)m",
                         replacement = "\\1t")

  word = str_replace_all(word,
                         pattern = "(m\\.ˈ?)n",
                         replacement = "\\1p")

  word = str_replace_all(word,
                         pattern = "(n\\.ˈ?)n",
                         replacement = "\\1d")

  word = str_replace_all(word,
                         pattern = "(m\\.ˈ?)m",
                         replacement = "\\1p")

  word = str_replace_all(word,
                         pattern = "(s\\.ˈ?)s",
                         replacement = "\\1t")

  word = str_replace_all(word,
                         pattern = "(l\\.ˈ?)l",
                         replacement = "\\1t")

  word = str_replace_all(word,
                         pattern = "(ɾ\\.ˈ?)ɾ",
                         replacement = "\\1d")

  return(word)
}


