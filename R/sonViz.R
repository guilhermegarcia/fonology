
#' Sonority plot
#'
#' Given a string, the function plots the sonority profile of the string using
#' the fine-grained sonority scale in Parker (2011)
#' @param word A phonemically transcribed word. Syllabification and stress are optional
#' @param syl Whether syllables should be color-coded (requires syllabification in the input using either . or -). Defaults to F
#' @param save_plot Whether the plot should be saved (in your working directory). Default is F
#' @return A ggplot figure with the sonority profile of the word
#' @examples
#' plotSon(word = "tɾon.fo.ˈnil.to", syl = T, save_plot = F));
#' @export

plotSon = function(word = "", syl = F, save_plot = F){
  if(!require("pacman", quietly = T)){install.packages("pacman")}
  pacman::p_load(tidyverse, RColorBrewer)

  lo_v = "a.ɶ.ɑ.ɒ.æ.ɐ" %>% str_split("\\.") %>% unlist()
  mid_per_v = "ɛ.œ.ʌ.ɔ.e.ø.ɤ.o" %>% str_split("\\.") %>% unlist()
  hi_per_v = "i.y.ɯ.u" %>% str_split("\\.") %>% unlist()
  mid_int_v = "ɘ.ɵ.ə.ɜ.ɞ" %>% str_split("\\.") %>% unlist()
  hi_int_v = "ɨ.ʉ" %>% str_split("\\.") %>% unlist()

  glides = "j.w.ɥ" %>% str_split("\\.") %>% unlist()

  rhotic_approx = "ɻ.ʀ.ɹ" %>% str_split("\\.") %>% unlist()
  flaps = "ɾ.ɽ.ⱱ." %>% str_split("\\.") %>% unlist()

  laterals = "l.ɬ.ɮ.ʎ" %>% str_split("\\.") %>% unlist()
  trills = "r.ʙ" %>% str_split("\\.") %>% unlist()

  nasals = "m.n.ɱ.ɳ.ɲ.ŋ.ɴ" %>% str_split("\\.") %>% unlist()

  fricatives_vce = "β.v.ð.z.ʒ.ʐ.ʝ.ɣ.ʁ.ʕ.ɦ" %>% str_split("\\.") %>% unlist()
  affricates_vce = "t͡s.d͡z.t͡ʃ.d͡ʒ" %>% str_split("\\.") %>% unlist()
  stops_vce = "b.d.g.β.ɖ.ɢ" %>% str_split("\\.") %>% unlist()
  fricatives = "ɸ.f.θ.s.ʃ.ʂ.ç.x.χ.ħ.h" %>% str_split("\\.") %>% unlist()
  affricates = "t͡s.t͡ʃ" %>% str_split("\\.") %>% unlist()
  stops = "p.t.k.ʈ.c.q.ʔ" %>% str_split("\\.") %>% unlist()

  # Complete
  full = tibble(phoneme = c(lo_v, mid_per_v, hi_per_v, mid_int_v, hi_int_v,
                            glides, rhotic_approx, flaps, laterals, trills,
                            nasals, fricatives_vce, affricates_vce, stops_vce,
                            fricatives, affricates, stops),
                son = c(rep(17, length(lo_v)),
                        rep(16, length(mid_per_v)),
                        rep(15, length(hi_per_v)),
                        rep(14, length(mid_int_v)),
                        rep(13, length(hi_int_v)),
                        rep(12, length(glides)),
                        rep(11, length(rhotic_approx)),
                        rep(10, length(flaps)),
                        rep(9, length(laterals)),
                        rep(8, length(trills)),
                        rep(7, length(nasals)),
                        rep(6, length(fricatives_vce)),
                        rep(5, length(affricates_vce)),
                        rep(4, length(stops_vce)),
                        rep(3, length(fricatives)),
                        rep(2, length(affricates)),
                        rep(1, length(stops))))

  checkInput = word %>%
    str_remove_all(pattern = "'|ˈ|ˌ|ː|ˑ|-|\\.") %>%
    str_split("") %>%
    unlist()

  absent = c()

  for(i in 1:length(checkInput)){
    if(!checkInput[i] %in% full$phoneme){
      absent[length(absent) + 1] = checkInput[i]
    }
  }

  if(length(absent) > 0){
    message("The following phonemes are not supported by the function:")
    return(absent)
  }



  if(syl){

    if(!str_detect(string = word, pattern = "\\.|-")){
      stop("Input must be syllabified.")
    }
    word_simple = word %>%
      str_remove_all("'|ˈ|ˌ|ː|ˑ") %>%
      str_split("") %>%
      unlist()


    word_son = tibble(phoneme = word_simple) %>%
      left_join(full, by = "phoneme") %>%
      mutate(item = str_c("item", row_number(), sep = "_"),
             syl = NA)

    syl_counter = 1

    for(i in 1:nrow(word_son)){
      if(!word_son$phoneme[i] %in% c(".", "-")){
        word_son$syl[i] = syl_counter
      } else if(word_son$phoneme[i] %in% c(".", "-")){
        word_son$syl[i] = NA
        syl_counter = syl_counter + 1
      }
    }

    word_son = word_son %>%
      mutate(syl = as.factor(syl)) %>%
      filter(!phoneme %in% c(".", "-"))

    sonPlot = ggplot(data = word_son, aes(x = item, y = son)) +
      geom_line(aes(group = 1), linetype = "twodash", color = "gray") +
      geom_point(aes(group = 1)) +
      coord_cartesian(ylim = c(0, 20)) +
      scale_x_discrete(labels = word_son$phoneme,
                       limits = as.character(word_son$item)) +
      theme_classic() +
      theme(axis.text = element_blank(), axis.line = element_blank(),
            axis.ticks = element_blank(),
            panel.background = element_blank(),
            legend.position = "none") +
      labs(y = NULL, x = NULL) +
      geom_label(aes(label = phoneme, group = item,
                     fill = syl),
                 size = 8, fontface = "bold",
                 label.r = unit(0, "lines"),
                 label.padding = unit(0.75, "lines")) +
      scale_fill_brewer(palette = "Pastel1")

    if(save_plot){
      ggsave(sonPlot, filename = "~/Desktop/sonPlot.jpeg", dpi = 1000, height = 5, width = nrow(word_son))
      message("The plot has been saved in your current working directory.")
    }


    return(sonPlot)
  }

  word_simple = word %>%
    str_remove_all("'|ˈ|ˌ|ː|ˑ|\\.|-") %>%
    str_split("") %>%
    unlist()

  word_son = tibble(phoneme = word_simple) %>%
    left_join(full, by = "phoneme") %>%
    mutate(item = str_c("item", row_number(), sep = "_"))

  sonPlot = ggplot(data = word_son, aes(x = item, y = son)) +
    geom_line(aes(group = 1), linetype = "dotdash") +
    geom_point(aes(group = 1)) +
    coord_cartesian(ylim = c(0, 20)) +
    scale_x_discrete(labels = word_son$phoneme,
                     limits = as.character(word_son$item)) +
    theme_classic() +
    theme(axis.text = element_blank(), axis.line = element_blank(),
          axis.ticks = element_blank(),
          panel.background = element_blank(),
          legend.position = "none") +
    labs(y = NULL, x = NULL) +
    geom_label(aes(label = phoneme, group = item),
               size = 8, fontface = "bold",
               label.r = unit(0, "lines"),
               label.padding = unit(0.75, "lines"))

  if(save_plot){
    ggsave(sonPlot, filename = "sonPlot.jpeg", dpi = 1000, height = 5, width = nrow(word_son))
    message("The plot has been saved in your current working directory.")
  }

  return(sonPlot)

}
