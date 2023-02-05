
stress_pt = function(word = ""){

  source("R/syllabify_pt.R")

  # Stress is final if word ends in consonant, diph OR high vowel (Tupi):
  word = str_replace_all(string = word,
                         pattern = "\\.(\\w+[pbtdkgszfvʃʒʎɲmnlɾwjiuãõw̃])$",
                         replacement = ".'\\1")

  word = str_replace_all(string = word,
                         pattern = "^(\\w*)$",
                         replacement = "'\\1")

  # Stress is antepenultimate if vowel is open:
  word = str_replace_all(string = word,
                         pattern = "(\\w*[ɔɛ]\\w*)(\\.\\w*\\.\\w*$)",
                         replacement = "'\\1\\2")

  # Else, penultimate stress:
  word = str_replace_all(string = word,
                         pattern = "(\\w+)(\\.\\w+)$",
                         replacement = "'\\1\\2")

  return(word)

}


pu_candidates = function(word = ""){

  c1 = str_replace(string = word,
                   pattern = "(\\w+\\.\\w+$)",
                   replacement = "'\\1")

  c2 = str_replace(string = word,
                   pattern = "(\\w+\\.)(\\w+$)",
                   replacement = "\\1'\\2")

  candidates = c(c1, c2)
  winner = sample(candidates, size = 1, prob = c(0.25, 0.75))

  return(winner)

}


apu_candidates = function(word = ""){

  c1 = str_replace(string = word,
                   pattern = "(\\w+\\.\\w+\\.\\w+$)",
                   replacement = "'\\1")

  c2 = str_replace(string = word,
                   pattern = "(\\w+\\.)(\\w+\\.\\w+$)",
                   replacement = "\\1'\\2")

  candidates = c(c1, c2)
  winner = sample(candidates, size = 1, prob = c(0.2, 0.8))

  return(winner)

}




# # for all words CV(C).CV.CV, antepenultimate stress is X% probable
#
#
# # Pick PSL:
load("/Users/gdgarcia/Dropbox/Website/quarto_website/PSL/psl.RData")

pt_lex = psl %>%
  select(word, pro) %>%
  mutate(pro = str_replace_all(pro, "-", "."),
         pro = str_replace_all(pro, "O", "ɔ"),
         pro = str_replace_all(pro, "E", "ɛ"),
         pro = str_replace_all(pro, "S", "ʃ"),
         pro = str_replace_all(pro, "Z", "ʒ"),
         pro = str_replace_all(pro, "r", "ɾ"),
         pro = str_replace_all(pro, "R", "x"),
         pro = str_replace_all(pro, "N", "ɲ"),
         pro = str_replace_all(pro, "a~w", "ãw̃"),
         pro = str_replace_all(pro, "a~", "ã"),
         pro = str_replace_all(pro, "L", "ʎ"))
#
# # usethis::use_data(pt_lex)
#
# Calculate proportions for APU:
# psl %>%
#   filter(nSyl > 2,
#          weightProfile %in% c("HLL", "LLL")) %>%
#   group_by(weightProfile, stressLoc) %>%
#   count() %>%
#   group_by(weightProfile) %>%
#   mutate(P = n / sum(n)) %>%
#   filter(stressLoc == "antepenult") %>%
#   select(-c(n, stressLoc))
# #
#
# # Calculate proportions for PU:
# psl %>%
#   filter(nSyl > 2,
#          weightProfile %in% c("LLH", "LH")) %>%
#   group_by(weightProfile, stressLoc) %>%
#   count() %>%
#   group_by(weightProfile) %>%
#   mutate(P = n / sum(n)) %>%
#   filter(stressLoc == "antepenult") %>%
#   select(-c(n, stressLoc))


#
# # candidates = c("átomo", "atómo")
#
# # sample(candidates, size = 1, prob = c(0.3, 0.7))
#
#
#
# transcribe_pt(word = "cavalo") %>%
#   syllabify_pt() %>%
#   stress_pt()
#
