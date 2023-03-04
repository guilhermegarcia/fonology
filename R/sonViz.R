
#' Sonority plot
#'
#' Given a string, the function plots the sonority profile of the string using
#' the fine-grained sonority scale in Parker (2011)
#' @param word A phonemically transcribed word. Syllabification and stress are optional
#' @param syl Whether syllables should be color-coded (requires syllabification in the input using either . or -). Defaults to F
#' @param save_plot Whether the plot should be saved (in your working directory). Default is F
#' @return A ggplot figure with the sonority profile of the word
#' @examples
#' plotSon(word = "tɾon.fo.ˈnil.to", syl = TRUE, save_plot = FALSE);
#' @importFrom magrittr %>%
#' @export

plotSon = function(word = "", syl = FALSE, save_plot = FALSE){

  lo_v = "a.\u0276.\u0251.\u0252.\u00e6.\u0250" %>% stringr::str_split("\\.") %>% unlist()
  mid_per_v = "\u025b.\u0153.\u028c.\u0254.e.\u00f8.\u0264.o" %>% stringr::str_split("\\.") %>% unlist()
  hi_per_v = "i.y.\u026f.u" %>% stringr::str_split("\\.") %>% unlist()
  mid_int_v = "\u0258.\u0275.\u0259.\u025c.\u025e" %>% stringr::str_split("\\.") %>% unlist()
  hi_int_v = "\u0268.\u0289" %>% stringr::str_split("\\.") %>% unlist()

  glides = "j.w.\u0265" %>% stringr::str_split("\\.") %>% unlist()

  rhotic_approx = "\u027b.\u0280.\u0279" %>% stringr::str_split("\\.") %>% unlist()
  flaps = "\u027e.\u027d.\u2c71." %>% stringr::str_split("\\.") %>% unlist()

  laterals = "l.\u026c.\u026e.\u028e" %>% stringr::str_split("\\.") %>% unlist()
  trills = "r.\u0299" %>% stringr::str_split("\\.") %>% unlist()

  nasals = "m.n.\u0271.\u0273.\u0272.\u014b.\u0274" %>% stringr::str_split("\\.") %>% unlist()

  fricatives_vce = "\u03b2.v.\u00f0.z.\u0292.\u0290.\u029d.\u0263.\u0281.\u0295.\u0266" %>% stringr::str_split("\\.") %>% unlist()
  affricates_vce = "t\u0361s.d\u0361z.t\u0361\u0283.d\u0361\u0292" %>% stringr::str_split("\\.") %>% unlist()
  stops_vce = "b.d.g.\u03b2.\u0256.\u0262" %>% stringr::str_split("\\.") %>% unlist()
  fricatives = "\u0278.f.\u03b8.s.\u0283.\u0282.\u00e7.x.\u03c7.\u0127.h" %>% stringr::str_split("\\.") %>% unlist()
  affricates = "t\u0361s.t\u0361\u0283" %>% stringr::str_split("\\.") %>% unlist()
  stops = "p.t.k.\u0288.c.q.\u0294" %>% stringr::str_split("\\.") %>% unlist()

  # Complete
  full = tibble::tibble(phoneme = c(lo_v, mid_per_v, hi_per_v, mid_int_v, hi_int_v,
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
    stringr::str_remove_all(pattern = "\'|\u02c8|\u02cc|\u02d0|\u02d1|-|\\.") %>%
    stringr::str_split("") %>%
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

    if(!stringr::str_detect(string = word, pattern = "\\.|-")){
      stop("Input must be syllabified.")
    }
    word_simple = word %>%
      stringr::str_remove_all("\'|\u02c8|\u02cc|\u02d0|\u02d1") %>%
      stringr::str_split("") %>%
      unlist()


    word_son = tibble::tibble(phoneme = word_simple) %>%
      dplyr::left_join(full, by = "phoneme") %>%
      dplyr::mutate(item = stringr::str_c("item", dplyr::row_number(), sep = "_"),
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
      dplyr::mutate(syl = as.factor(syl)) %>%
      dplyr::filter(!phoneme %in% c(".", "-"))

    sonPlot = ggplot2::ggplot(data = word_son, ggplot2::aes(x = item, y = son)) +
      ggplot2::geom_line(ggplot2::aes(group = 1), linetype = "twodash", color = "gray") +
      ggplot2::geom_point(ggplot2::aes(group = 1)) +
      ggplot2::coord_cartesian(ylim = c(0, 20)) +
      ggplot2::scale_x_discrete(labels = word_son$phoneme,
                                limits = as.character(word_son$item)) +
      ggplot2::theme_classic() +
      ggplot2::theme(axis.text = ggplot2::element_blank(), axis.line = ggplot2::element_blank(),
                     axis.ticks = ggplot2::element_blank(),
                     panel.background = ggplot2::element_blank(),
                     legend.position = "none") +
      ggplot2::labs(y = NULL, x = NULL) +
      ggplot2::geom_label(ggplot2::aes(label = phoneme, group = item,
                              fill = syl),
                          size = 8, fontface = "bold",
                          label.r = ggplot2::unit(0, "lines"),
                          label.padding = ggplot2::unit(0.75, "lines")) +
      ggplot2::scale_fill_brewer(palette = "Pastel1")

    if(save_plot){
      ggplot2::ggsave(sonPlot, filename = "~/Desktop/sonPlot.jpeg", dpi = 1000, height = 5, width = nrow(word_son))
      message("The plot has been saved in your current working directory.")
    }


    return(sonPlot)
  }

  word_simple = word %>%
    stringr::str_remove_all("\'|\u02c8|\u02cc|\u02d0|\u02d1|\\.|-") %>%
    stringr::str_split("") %>%
    unlist()

  word_son = tibble::tibble(phoneme = word_simple) %>%
    dplyr::left_join(full, by = "phoneme") %>%
    dplyr::mutate(item = stringr::str_c("item", dplyr::row_number(), sep = "_"))

  sonPlot = ggplot2::ggplot(data = word_son, ggplot2::aes(x = item, y = son)) +
    ggplot2::geom_line(ggplot2::aes(group = 1), linetype = "dotdash") +
    ggplot2::geom_point(ggplot2::aes(group = 1)) +
    ggplot2::coord_cartesian(ylim = c(0, 20)) +
    ggplot2::scale_x_discrete(labels = word_son$phoneme,
                              limits = as.character(word_son$item)) +
    ggplot2::theme_classic() +
    ggplot2::theme(axis.text = ggplot2::element_blank(), axis.line = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank(),
                   panel.background = ggplot2::element_blank(),
                   legend.position = "none") +
    ggplot2::labs(y = NULL, x = NULL) +
    ggplot2::geom_label(ggplot2::aes(label = phoneme, group = item),
                        size = 8, fontface = "bold",
                        label.r = ggplot2::unit(0, "lines"),
                        label.padding = ggplot2::unit(0.75, "lines"))

  if(save_plot){
    ggplot2::ggsave(sonPlot, filename = "sonPlot.jpeg", dpi = 1000, height = 5, width = nrow(word_son))
    message("The plot has been saved in your current working directory.")
  }

  return(sonPlot)

}
