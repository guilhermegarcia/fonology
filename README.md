
<!-- README.md is generated from README.Rmd. Please edit that file -->

<!-- README.md is generated from README.Rmd. Please edit that file -->

<p>

<a href = "https://github.com/guilhermegarcia/fonology"><img src = "https://img.shields.io/github/r-package/v/guilhermegarcia/fonology" alt="R version"></a>
<a href = "https://github.com/guilhermegarcia/fonology"><img src = "https://img.shields.io/github/last-commit/guilhermegarcia/fonology"></a>
<a href = "https://github.com/guilhermegarcia/fonology/issues"><img src = "https://img.shields.io/github/issues/guilhermegarcia/fonology"></a>
<a href = "https://gdgarcia.ca/fonology/" target = "_blank"><img src = "https://img.shields.io/badge/Package-website-critical"></a>
<a href = "https://osf.io/7ta53/" target = "_blank"><img src = "https://img.shields.io/badge/OSF-Repo-9cf"></a>
</p>

<!-- [![DOI](https://zenodo.org/badge/597557920.svg)](https://zenodo.org/badge/latestdoi/597557920) -->

## Fonology

> An R package for phonological analysis

For a more comprehensive vignette, visit [my
website](https://gdgarcia.ca/fonology). The package requires `R >= 4.1`.

## Main functions and data

<a href = "https://gdgarcia.ca/fonology"><img align="right" height="150" style="float:right; margin:0px 50px" src="https://gdgarcia.ca/figures/fonology.png"></a>

- `getFeat()` and `getPhon()` to work with distinctive features
- `ipa()` phonemically transcribes words (real or not) in Portuguese,
  French or Spanish
  - `ipa_pt()` offers a more detailed transcription for Portuguese
- `maxent()` implements a MaxEnt grammar given a tableau object
- `nhg()` and `plotNgh()` can be used to generate and visualize
  probabilities for candidates in a Noisy Harmonic Grammar
- `sonDisp()` calculates the sonority dispersion of a given
  demisyllable. `meanSonDisp()` calculates the average dispersion for a
  given word (or vector of words)
- `wug_pt()` generates hypothetical words in Portuguese
- `biGram_pt()` calculates bigram probabilities for a given word
- `plotVowels()` generates vowel trapezoids
- `plotSon()` plots the sonority profile of a given word
- `psl` contains the [*Portuguese Stress
  Lexicon*](https://gdgarcia.ca/psl.html)
- `pt_lex` contains a simplified version of `psl`
- `stopwords_pt`, `stopwords_fr` and `stopwords_sp` contain stopwords in
  Portuguese, French and Spanish

## Distinctive features

### From phonemes to features

The function `getFeat()` requires a set of phonemes `ph` and a language
`lg`. It outputs the minimal matrix of distinctive features for `ph`
given the phonemic inventory of `lg`. Five languages are supported:
English, French, Italian, Portuguese, and Spanish. You can also use `lg`
to provide your own phonemic inventory as a vector. Here are some
examples.

``` r
library(Fonology)
getFeat(ph = c("i", "u"), lg = "English")
#> [1] "+hi"    "+tense"
getFeat(ph = c("i", "u"), lg = "French")
#> [1] "Not a natural class in this language."
getFeat(ph = c("i", "y", "u"), lg = "French")
#> [1] "+syl" "+hi"
getFeat(ph = c("p", "b"), lg = "Portuguese")
#> [1] "-son"  "-cont" "+lab"
getFeat(ph = c("k", "g"), lg = "Italian")
#> [1] "+cons" "+back"
```

### From features to phonemes

The function `getPhon()` requires a feature matrix `ft` and a language
`lg`. It outputs the set of phonemes represented by `ft` given the
phonemic inventory of `lg`. The languages supported are the same as
those supported by `getFeat()`, and you can again use `lg` to provide
your own phonemic inventory as a vector.

``` r
getPhon(ft = c("+syl", "+hi"), lg = "French")
#> [1] "u" "i" "y"
getPhon(ft = c("-DR", "-cont", "-son"), lg = "English")
#> [1] "t" "d" "b" "k" "g" "p"
getPhon(ft = c("-son", "+vce"), lg = "Spanish")
#> [1] "z" "d" "b" "ʝ" "g" "v"
```

## IPA transcription

The function `ipa()` takes a `word` (or a vector with multiple words,
real or not) in Portuguese, French or Spanish in its orthographic form
and returns its phonemic (i.e., broad) transcription. The accuracy of
grapheme-to-phoneme conversion is at least 80% for all three languages.
Narrow transcription is available for Portuguese (based on Brazilian
Portuguese), which includes secondary stress—this can be generated by
adding `narrow = T` to the function. Run `ipa_pt_test()`,
`ipa_fr_test()` and `ipa_sp_test()` for sample words in both languages.
By default, `ipa()` assumes that `lg = "Portuguese"` (or `lg = "pt"`)
and `narrow = F`.

``` r
ipa("atletico")
#> [1] "a.tle.ˈti.ko"
ipa("cantalo", narrow = T)
#> [1] "kãn.ˈta.lʊ"
ipa("antidepressivo", narrow = T)
#> [1] "ˌãn.t͡ʃi.ˌde.pɾe.ˈsi.vʊ"
ipa("feris")
#> [1] "fe.ˈris"
ipa("mejorado", lg = "sp")
#> [1] "me.xo.ˈɾa.do"
ipa("nuevos", lg = "sp")
#> [1] "nu.ˈe.bos"
ipa("informatique", lg = "fr")
#> [1] "ɛ̃.fɔʁ.ma.tik"
ipa("acheter", lg = "fr")
#> [1] "a.ʃə.te"
```

### A note on stress

A more detailed function, `ipa_pt()`, is available for Portuguese only.
In it, stress is assigned based on two scenarios. First, **real** words
(non-verbs) have their stress assignment derived from the [Portuguese
Stress Lexicon](psl.html) (Garcia 2014)—if the word is listed there.
Second, **nonce** words follow the general patterns of Portuguese stress
*as well as* probabilistic tendencies shown in my work (Garcia, 2017a,
2017b, 2019). As a result, a nonce word *may* have antepenultimate
stress under the right conditions based on lexical statistics in the
language. Likewise, words with other so-called exceptional stress
patterns are also generated probabilistically (e.g., `LH]` words with
penultimate stress). Stress and weight are also used to apply both
spondaic and dactylic lowering to narrow transcriptions, following work
such as Wetzels (2007). Secondary stress is provided when `narrow = T`.
In the function `ipa()`, stress is *not* probabilistic (and therefore
not variable): it merely follows the orthography as well as the typical
stress rules in Portuguese (and Spanish).

### A note on key assumptions

There are several assumptions about surface-forms when `narrow = T`
(Portuguese only). Most of these assumptions can (and probably will) be
adjusted as the package improves its accuracy and coverage.
Diphthongization, for example, is sensitive to phonotactics. A word such
as `CV.ˈV.CV` will be narrowly transcribed as `ˈCGV.CV` (except when the
initial consonant is an affricate (allophonic), which *seems* to lower
the probability of diphthongization based on my judgement).
Diphthongization is not applied if the onset is complex. Needless to
say, these assumptions are based on a particular dialect of Brazilian
Portuguese, and I do not expect all of them to seamlessly apply to other
dialects (although some assumptions are more easily generalizable than
others).

Narrow transcription also includes (final) vowel reduction, voicing
assimilation, l-vocalization, vowel devoicing, palatalization, and
epenthesis in `sC` clusters and other consonant sequences that are
expected to be repaired on surface forms (e.g., *kt*, *gn*). Examples
can be generated with the function `ipa_pt_test()`.

### Helper functions

If you plan to tokenize texts and create a table with individual columns
for stress and syllables, you can use some simple additional helper
functions. For example, `getWeight()` will take a syllabified word and
return its weight profile (e.g., `getWeight("kon.to")` will return
`HL`). The function `getStress()`[^1] will return the stress position of
a given word (up to preantepenultimate stress)—the word must already be
stressed, but the symbol used can be specified in the function (argument
`stress`). Finally, `countSyl()` will return the number of syllables in
a given string, and `getSyl()` will extract a particular syllable from a
string. For example,
`getSyl(word = "kom-pu-ta-doɾ", pos = 3, syl = "-")` will take the
antepenultimate syllable of the string in question. The default symbol
for syllabification is the period.

## Probabilistic grammars

The function `maxent()` learns weights given a tableau object containing
inputs, outputs, constraints, violations and observations (see
documentation). The function returns a list with different objects,
including learned weights, BIC value and predicted probabilities for
each output. If the reader wishes to pursue a comprehensive MaxEnt
analysis, I strongly recommend the `maxent.ot` package, which is
dedicated exclusively to MaxEnt grammars (Mayer et al, 2024). Here’s an
example of `maxent()` in action.

``` r
maxent_data <- tibble::tibble(
  input = rep(c("pad", "tab", "bid", "dog", "pok"), each = 2),
  output = c("pad", "pat", "tab", "tap", "bid", "bit", "dog", "dok", "pog", "pok"),
  ident_vce = c(0, 1, 0, 1, 0, 1, 0, 1, 1, 0),
  no_vce_final = c(1, 0, 1, 0, 1, 0, 1, 0, 1, 0),
  obs = c(5, 15, 10, 20, 12, 18, 12, 17, 4, 8)
)
maxent(tableau = maxent_data)
#> $predictions
#> # A tibble: 10 × 12
#>    input output ident_vce no_vce_final   obs harmony max_h exp_h     Z obs_prob
#>    <chr> <chr>      <dbl>        <dbl> <dbl>   <dbl> <dbl> <dbl> <dbl>    <dbl>
#>  1 pad   pad            0            1     5  0.639  0.639  1     2.79    0.25 
#>  2 pad   pat            1            0    15  0.0541 0.639  1.79  2.79    0.75 
#>  3 tab   tab            0            1    10  0.639  0.639  1     2.79    0.333
#>  4 tab   tap            1            0    20  0.0541 0.639  1.79  2.79    0.667
#>  5 bid   bid            0            1    12  0.639  0.639  1     2.79    0.4  
#>  6 bid   bit            1            0    18  0.0541 0.639  1.79  2.79    0.6  
#>  7 dog   dog            0            1    12  0.639  0.639  1     2.79    0.414
#>  8 dog   dok            1            0    17  0.0541 0.639  1.79  2.79    0.586
#>  9 pok   pog            1            1     4  0.693  0.693  1     3.00    0.333
#> 10 pok   pok            0            0     8  0      0.693  2.00  3.00    0.667
#> # ℹ 2 more variables: pred_prob <dbl>, error <dbl>
#> 
#> $weights
#>    ident_vce no_vce_final 
#>   0.05410679   0.63904039 
#> 
#> $log_likelihood
#> [1] -78.72152
#> 
#> $log_likelihood_norm
#> [1] -7.872152
#> 
#> $bic
#> [1] -152.8379
```

Finally, a couple of functions are dedicated to Noisy Harmonic Grammars.
These are pedagogical tools that can be used to demonstrate how
probabilities are generated given constraint weights and violation
profiles for different candidates. The function `nhg()` takes a tableau
object and returns predicted probabilities given `n` simulations. The
user can also set the standard deviation for the noise used. The
function `plotNhg()` can be helpful to visualize how different standard
deviations affect probabilities over candidates after 100 simulations.

## Sonority

There are three functions in the package to analyze sonority. First,
`demi(word = ..., d = ...)` extracts either the first (`d = 1`, the
default) or second (`d = 2`) demisyllables of a given (syllabified) word
(or vector of words. Second, `sonDisp(demi = ...)` calculates the
sonority dispersion score of a given demisyllable, based on Clements
(1990)—see also Parker (2011). Note that this metric does not
differentiate sequences that respect the sonority sequencing principle
(SSP) from those that don’t, i.e., `pla` and `lpa` will have the same
score. For that reason, a third function exists,
`ssp(demi = ..., d = ...)`, which evaluates whether a given demisyllable
respects (`1`) or doesn’t respect (`0`) the SSP. In the example below,
the dispersion score of the first demisyllable in the penult syllable is
calculated—`ssp()` isn’t relevant here, since all words in Portuguese
respect the SSP.

``` r
example = tibble(word = c("partolo", "metrilpo", "vanplidos"))

example = example |>
  rowwise() |>
  mutate(ipa = ipa(word),
         syl2 = getSyl(word = ipa, pos = 2),
         demi1 = demi(word = syl2, d = 1),
         disp = sonDisp(demi = demi1),
         SSP = ssp(demi = demi1, d = 1))

example
#> # A tibble: 3 × 6
#> # Rowwise: 
#>   word      ipa          syl2  demi1  disp   SSP
#>   <chr>     <chr>        <chr> <chr> <dbl> <dbl>
#> 1 partolo   par.ˈto.lo   to    to     0.06     1
#> 2 metrilpo  me.ˈtril.po  tril  tri    0.56     1
#> 3 vanplidos vam.ˈpli.dos pli   pli    0.56     1
```

You may also want to calculate the average sonority dispersion for whole
words with the function `meanSonDisp()`. If your words of interest are
possible or real Portuguese words, they can be entered in their
orthographic form. Otherwise, they need to be phonemically transcribed
and syllabified. In this scenario, use `phonemic = T`.

``` r
meanSonDisp(word = c("partolo", "metrilpo", "vanplidos"))
#> [1] 1.53
```

## Plotting vowels

The function `plotVowels()` creates a vowel trapezoid using `ggplot2`
and also returns the LaTeX code to create the same trapezoid using the
[`vowel` package](https://ctan.org/pkg/vowel?lang=en). Available
languages: Arabic, French, English, Dutch, German, Hindi, Italian,
Japanese, Korean, Mandarin, Portuguese, Spanish, Swahili, Russian,
Talian, Thai, and Vietnamese

## Bigram probabilities

The function `biGram_pt()` returns the log bigram probability for a
possible `word` in Portuguese (`word` must be broadly transcribed). The
string must use broad phonemic transcription, but no syllabification or
stress. The reference used calculate probabilities is the [Portuguese
Stress Lexicon](https://gdgarcia.ca/psl.html).

Two additional functions can be used to explore bigrams: `nGramTbl()`
generates a tibble with phonotactic bigrams from a given text, and
`plotnGrams()` creates a plot for inputs generated with `nGramTbl()`.

## Word generator for Portuguese

The function `wug_pt()` generates a hypothetical word in Portuguese.
Note that this function is meant to be used to get you started with
nonce words. You will most likely want to make adjustments based on
phonotactic preferences. The function already takes care of *some* OCP
effects and it also prohibits more than one onset cluster per word,
since that’s relatively rare in Portuguese. Still, there will certainly
be other sequences that sound less natural. The function is not too
strict because you may have a wide range of variables in mind as you
create novel words. Finally, if you wish to include palatalization, set
`palatalization = T`—if you do that, bear in mind that `biGram_pt()`
won’t work as it requires phonemic transcription without syllabification
or stress.

``` r
set.seed(1)
wug_pt(profile = "LHL")
#> [1] "dra.ˈbur.me"

# Let's create a table with 5 nonce words and their bigram probability
set.seed(1)
tibble(word = character(5)) |>
  mutate(word = wug_pt("LHL", n = 5),
         bigram = word |> biGram_pt())
#> # A tibble: 5 × 2
#>   word        bigram
#>   <chr>        <dbl>
#> 1 dra.ˈbur.me -119. 
#> 2 ze.ˈfran.ka  -85.6
#> 3 be.ˈʒan.tre  -84.8
#> 4 ʒa.ˈgran.fe  -87.6
#> 5 me.ˈxes.vro -101.
```

## Extras

Additional functions include `monthsAge()` and `meanAge()`, both of
which can be used to convert and analyze ages following the format
`yy;mm`, commonly used in first language acquisition studies. It’s a
good idea to check out the index of functions (`?Fonology`) to take a
look at the complete list of functions available.

## Acknowledgements and funding

Parts of this project have benefited from funding from the ENVOL program
at Université Laval and from the Social Sciences and Humanities Research
Council of Canada (**SSHRC**). Different undergraduate research
assistants at Université Laval have worked on the Spanish and French
grapheme-to-phoneme conversion functions: Nicolas C. Bustos, Emmy
Dumont, and Linda Wong. Matéo Levesque implemented comprehensive regular
expressions for French transcription.

## References

- Clements, G. N. (1990). The role of the sonority cycle in core
  syllabification. *In* John Kingston & Mary E. Beckman (eds.) *Papers
  in laboratory phonology I: Between the grammar and physics of speech*,
  283–333. Cambridge: Cambridge University Press.

- Garcia, G. D. (2014). *Portuguese Stress Lexicon*. Available at
  [gdgarcia.ca/psl.html](https://gdgarcia.ca/psl.html).

- Garcia, G. D. (2017). *Weight effects on stress: Lexicon and grammar*.
  PhD thesis, McGill University. <https://doi.org/10.31219/osf.io/bt8hk>

- Garcia, G. D. (2017). Weight gradience and stress in Portuguese.
  *Phonology*, 34(1), 41–79. <https://doi.org/10.1017/S0952675717000033>

- Garcia, G. D. (2019). When lexical statistics and the grammar
  conflict: Learning and repairing weight effects on stress. *Language*,
  95(4), 612–641. <https://doi.org/10.1353/lan.2019.0068>

- Mayer, C., Tan, A., & Zuraw, K. R. (2024). Introducing `maxent.ot`: An
  R package for Maximum Entropy constraint grammars. *Phonological Data
  and Analysis*, 6(4), 1–44. <https://doi.org/10.3765/pda.v6art4.88>

- Parker, S. (2011). Sonority. In M. van Oostendorp, C. J. Ewen, E.
  Hume, & K. Rice (Eds.), *The Blackwell companion to phonology*
  (pp. 1160–1184). Wiley Online Library.
  <https://doi.org/10.1002/9781444335262.wbctp0049>

- Wetzels, L., (2007) Primary Word Stress in Brazilian Portuguese and
  the Weight Parameter, *Journal of Portuguese Linguistics* 6(1), 9-58.
  doi: <https://doi.org/10.5334/jpl.144>

[^1]: Functions without `_pt`, `_fr` or `_sp` are language-independent.
