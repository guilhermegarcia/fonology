#' Vowel plot generator
#'
#' Generates vowel trapezoid using ggplot2 as well as LaTeX code using vowel package.
#' @param lg The language whose vowel inventory will be plotted
#' @param tex Whether a latex file is desired. By deafult, tex = FALSE
#' @return The vowel inventory desired
#' @examples
#' plotVowels(lg = "portuguese");
#' plotVowels(lg = "english");
#' @export

plotVowels = function(lg = "English", tex = F){

  if (!require("pacman", quietly = T)) install.packages("pacman")
  pacman::p_load(tidyverse)

  # Tex file:
  texOutput = "\\documentclass[12pt, letterpaper]{article}
\\usepackage{vowel}
\\usepackage{tipa}
\\begin{document}
{\\LARGE
\\begin{center}
\\begin{vowel}
 \\putcvowel[l]{\\textipa{i}}{1}
 \\putcvowel[r]{\\textipa{y}}{1}
 \\putcvowel[l]{\\textipa{e}}{2}
 \\putcvowel[r]{\\textipa{\\o}}{2}
 \\putcvowel[l]{\\textipa{E}}{3}
 \\putcvowel[r]{\\textipa{\\oe}}{3}
 \\putcvowel[l]{\\textipa{a}}{4}
 \\putcvowel[r]{\\textscoelig}{4}
 \\putcvowel[l]{\\textipa{A}}{5}
 \\putcvowel[r]{\\textipa{6}}{5}
 \\putcvowel[l]{\\textipa{2}}{6}
 \\putcvowel[r]{\\textipa{O}}{6}
 \\putcvowel[l]{\\textipa{7}}{7}
 \\putcvowel[r]{\\textipa{o}}{7}
 \\putcvowel[l]{\\textipa{W}}{8}
 \\putcvowel[r]{\\textipa{u}}{8}
 \\putcvowel[l]{\\textipa{1}}{9}
 \\putcvowel[r]{\\textipa{0}}{9}
 \\putcvowel[l]{\\textipa{9}}{10}
 \\putcvowel[r]{\\textipa{8}}{10}
 \\putcvowel{\\textipa{@}}{11}
 \\putcvowel[l]{\\textipa{3}}{12}
 \\putcvowel[r]{\\textcloserevepsilon}{12}
 \\putcvowel{\\textipa{I}\\ \\textipa{ }}{13}
 \\putcvowel{\\textipa{ }\\ \\textipa{Y}}{13}
 \\putcvowel{\\textipa{U}}{14}
 \\putcvowel{\\textipa{5}}{15}
 \\putcvowel{\\textipa{\\ae}}{16}
\\end{vowel}
\\end{center}
}
\\end{document}"


# Tex tibble:
latex = tibble(lines = str_split(texOutput, "\\n")[[1]],
               vowel = c(rep("pre", 7),
                         c("i", "y", "e", "ø",
                           "ɛ", "œ", "a", "ɶ",
                           "ɑ", "ɒ", "ʌ", "ɔ",
                           "ɤ", "o", "ɯ", "u",
                           "ɨ", "ʉ", "ɘ", "ɵ",
                           "ə", "ɜ", "ɞ", "ɪ",
                           "ʏ", "ʊ", "ɐ", "æ"),
                         rep("post", 4)))

# Tibble:
V = tibble(vowel = character(), x = numeric(), y = numeric())

V = V %>%
  add_row(vowel = "a",
          x = 3.4,
          y = -0.9) %>%
  add_row(vowel = "ɛ",
          x = 1.7,
          y = 3.1) %>%
  add_row(vowel = "ɔ",
          x = 11.7,
          y = 3.1) %>%
  add_row(vowel = "e",
          x = 0,
          y = 7.1) %>%
  add_row(vowel = "o",
          x = 11.7,
          y = 7.1) %>%
  add_row(vowel = "i",
          x = -1.5,
          y = 11.1) %>%
  add_row(vowel = "u",
          x = 11.7,
          y = 11.1) %>%

  add_row(vowel = "ɶ",
          x = 4.8,
          y = -0.9) %>%
  add_row(vowel = "ɑ",
          x = 10.2,
          y = -0.9) %>%
  add_row(vowel = "ɒ",
          x = 11.7,
          y = -0.9) %>%

  add_row(vowel = "æ",
          x = 2.5,
          y = 1) %>%
  add_row(vowel = "ɐ",
          x = 6.65,
          y = 1.3) %>%
  add_row(vowel = "œ",
          x = 3.1,
          y = 3.1) %>%

  add_row(vowel = "ɜ",
          x = 5.8,
          y = 3.1) %>%
  add_row(vowel = "ɞ",
          x = 7,
          y = 3.1) %>%
  add_row(vowel = "ʌ",
          x = 10.2,
          y = 3.1) %>%

  add_row(vowel = "ø",
          x = 1.35,
          y = 7.1) %>%
  add_row(vowel = "ɘ",
          x = 5,
          y = 7.1) %>%
  add_row(vowel = "ɵ",
          x = 6.4,
          y = 7.1) %>%
  add_row(vowel = "ə",
          x = 6,
          y = 5.1) %>%
  add_row(vowel = "ɤ",
          x = 10.2,
          y = 7.1) %>%
  add_row(vowel = "ɪ",
          x = 1.4,
          y = 9.25) %>%
  add_row(vowel = "ʏ",
          x = 2.4,
          y = 9.25) %>%

  add_row(vowel = "y",
          x = -0.28,
          y = 11.1) %>%
  add_row(vowel = "ɨ",
          x = 4.3,
          y = 11.1) %>%
  add_row(vowel = "ʉ",
          x = 5.8,
          y = 11.1) %>%

  add_row(vowel = "ɯ",
          x = 10.1,
          y = 11.1) %>%
  add_row(vowel = "ʊ",
          x = 9,
          y = 9.25)

V = V %>%
  mutate(choice = row_number())

# Language inventories:

arabic = c("a", "i", "u")
french = c("a", "e", "ø", "ɑ", "i", "y", "o", "u", "ɛ", "ɔ", "ə", "œ")
english = c("a", "e", "ɑ", "i", "o", "u", "ɛ", "ɔ", "ə", "ɪ", "ʊ", "æ", "ʌ")
dutch = c("a", "e", "i", "o", "u", "ɛ", "ɔ", "ə", "ɪ", "ʏ", "ɐ", "y", "ø")
german = c("a", "e", "œ", "i", "o", "u", "ɛ", "ɔ", "ə", "ɪ", "ʊ", "ʏ", "ɐ", "y", "ø")
hindi = c("ɑ", "e", "i", "o", "u", "ɛ", "ɔ", "ɪ", "ʊ", "ə")
italian = c("a", "e", "i", "o", "u", "ɛ", "ɔ")
japanese = c("a", "e", "i", "o", "ɯ")
korean = c("ɐ", "e", "i", "o", "ɯ", "u", "ʌ")
mandarin = c("i", "y", "u", "ɤ", "a", "ə")
portuguese = c("a", "e", "i", "o", "u", "ɛ", "ɔ")
spanish = c("a", "e", "i", "o", "u")
swahili = c("a", "i", "u", "ɛ", "ɔ")
russian = c("a", "e", "i", "o", "u", "ɨ")
talian = c("a", "e", "i", "o", "u", "ɛ", "ɔ")
thai = c("a", "e", "i", "o", "u", "ɛ", "ɔ", "ɯ", "ɤ")
vietnamese = c("a", "e", "i", "o", "u", "ɛ", "ɔ", "ɯ", "ɤ")



if(lg == "all"){

  fig = ggplot(data = V, aes(x = x, y = y, label = vowel)) +
    labs(x = NULL,
         y = NULL) +
    theme_classic() +
    theme(axis.ticks = element_blank(),
          axis.text = element_blank(),
          line = element_blank()) +
    geom_segment(aes(x = 7, y = -1, xend = 5, yend = 11), color = "gray") +       # center vertical
    geom_segment(aes(x = 3, y = 3, xend = 5.3, yend = 3), color = "gray")  +      # mid-low left
    geom_segment(aes(x = 7, y = 3, xend = 10, yend = 3), color = "gray")  +       # mid-low right
    geom_segment(aes(x = 1.35, y = 7, xend = 5, yend = 7), color = "gray") +      # mid-hi left
    geom_segment(aes(x = 6.4, y = 7, xend = 10, yend = 7), color = "gray") +      # mid-hi right
    geom_segment(aes(x = 4.7, y = -1, xend = 10, yend = -1), color = "gray40") +  # bottom
    geom_segment(aes(x = 0, y = 11, xend = 4, yend = 11), color = "gray40") +     # top left
    geom_segment(aes(x = 6, y = 11, xend = 10, yend = 11), color = "gray40") +    # top right
    geom_label(size = 10, color = "black", label.size = 0, fill = "white",
               # family = "Charis SIL",
               label.r = unit(1, "lines"),
               label.padding = unit(0.5, "lines")) +
    geom_segment(aes(x = 4, y = -1, xend = -1, yend = 11), color = "gray40") +    # left
    geom_segment(aes(x = 11, y = 11, xend = 11, yend = -1), color = "gray40") +   # right
    geom_point(aes(x = 4, y = -1), size = 5, color = "gray40") +
    geom_point(aes(x = 11, y = -1), size = 5, color = "gray40") +
    geom_point(aes(x = 2.33, y = 3), size = 5, color = "gray40") +
    geom_point(aes(x = 6.35, y = 3), size = 5, color = "gray40") +
    geom_point(aes(x = 11, y = 3), size = 5, color = "gray40") +
    geom_point(aes(x = 0.67, y = 7), size = 5, color = "gray40") +
    geom_point(aes(x = 5.68, y = 7), size = 5, color = "gray40") +
    geom_point(aes(x = 11, y = 7), size = 5, color = "gray40") +
    geom_point(aes(x = -1, y = 11), size = 5, color = "gray40") +
    geom_point(aes(x = 5, y = 11), size = 5, color = "gray40") +
    geom_point(aes(x = 11, y = 11), size = 5, color = "gray40")


  texOutput = latex %>%
    # filter(vowel %in% c(targetLanguage, "pre", "post")) %>%
    pull(lines) %>%
    str_c(collapse = "\n")

  print(str_c("LaTeX code: ", texOutput))

  return(fig)

} else {

  targetLanguage = eval(parse(text = str_to_lower(lg)))

  fig = ggplot(data = V %>%
                 filter(vowel %in% targetLanguage) %>%
                 droplevels(), aes(x = x, y = y, label = vowel)) +
    labs(x = NULL,
         y = NULL) +
    theme_classic() +
    theme(axis.ticks = element_blank(),
          axis.text = element_blank(),
          line = element_blank()) +
    geom_segment(aes(x = 7, y = -1, xend = 5, yend = 11), color = "gray") +       # center vertical
    geom_segment(aes(x = 3, y = 3, xend = 5.3, yend = 3), color = "gray")  +      # mid-low left
    geom_segment(aes(x = 7, y = 3, xend = 10, yend = 3), color = "gray")  +       # mid-low right
    geom_segment(aes(x = 1.35, y = 7, xend = 5, yend = 7), color = "gray") +      # mid-hi left
    geom_segment(aes(x = 6.4, y = 7, xend = 10, yend = 7), color = "gray") +      # mid-hi right
    geom_segment(aes(x = 4.7, y = -1, xend = 10, yend = -1), color = "gray40") +  # bottom
    geom_segment(aes(x = 0, y = 11, xend = 4, yend = 11), color = "gray40") +     # top left
    geom_segment(aes(x = 6, y = 11, xend = 10, yend = 11), color = "gray40") +    # top right
    geom_label(size = 10, color = "black", label.size = 0, fill = "white",
               # family = "Charis SIL",
               label.r = unit(1, "lines"),
               label.padding = unit(0.5, "lines")) +
    geom_segment(aes(x = 4, y = -1, xend = -1, yend = 11), color = "gray40") +    # left
    geom_segment(aes(x = 11, y = 11, xend = 11, yend = -1), color = "gray40") +   # right
    geom_point(aes(x = 4, y = -1), size = 5, color = "gray40") +
    geom_point(aes(x = 11, y = -1), size = 5, color = "gray40") +
    geom_point(aes(x = 2.33, y = 3), size = 5, color = "gray40") +
    geom_point(aes(x = 6.35, y = 3), size = 5, color = "gray40") +
    geom_point(aes(x = 11, y = 3), size = 5, color = "gray40") +
    geom_point(aes(x = 0.67, y = 7), size = 5, color = "gray40") +
    geom_point(aes(x = 5.68, y = 7), size = 5, color = "gray40") +
    geom_point(aes(x = 11, y = 7), size = 5, color = "gray40") +
    geom_point(aes(x = -1, y = 11), size = 5, color = "gray40") +
    geom_point(aes(x = 5, y = 11), size = 5, color = "gray40") +
    geom_point(aes(x = 11, y = 11), size = 5, color = "gray40")

  # ADJUST TEX:
  texOutput = latex %>%
    filter(vowel %in% c(targetLanguage, "pre", "post")) %>%
    pull(lines) %>%
    str_c(collapse = "\n")

  if(tex == T){
    write_lines(texOutput, file = "vowels.tex")
    print("'vowels.tex' exported to your current working directory.")
  }
  return(fig)

}


}
