#' Data: Portuguese Stress Lexicon - simplified
#'
#' Comprehensive list of non-verbs in Portuguese and their phonemic transcription.
#' This data set is used by different functions in Fonology. Unlike the original PSL data base,
#' \code{pt_lex} uses slightly different transcription conventions.
#'
#' @format A data frame with 154,610 rows and 2 columns:
#' \describe{
#'   \item{\code{word}}{A word in its orthographic form}
#'   \item{\code{pro}}{The phonemic transcription of a word}
#' }
#' @usage data(pt_lex)
#' @author Guilherme D. Garcia (\url{https://gdgarcia.ca})
#' @source <https://gdgarcia.ca/psl>
"pt_lex"

#' Data: Portuguese Stress Lexicon
#'
#' The Portuguese Stress Lexicon (PSL; Garcia (2014)) contains non-verbs in the Portuguese language (excluding monosyllables). The lexicon is largely based on the list of words in the Houaiss Dictionary (Houaiss et al. 2001), which is the most comprehensive dictionary in Portuguese. PSL contains 154,610 entries and 62 columns, which provide a comprehensive set of variables (including pronunciation, syllabification, stress position, syllabic constituents, intervals, CV profiles and weight profiles).
#' Comprehensive list of non-verbs in Portuguese.
#' The lexicon is coded for a number of phonological variables.
#'
#' @usage data(psl)
#' @format A data frame with 154,610 rows and 62 columns:
#' \describe{
#'   \item{\code{word}}{A word in its orthographic form}
#'   \item{\code{pro}}{The phonemic transcription of a word. This uses slightly different conventions than those in Fonology transcriptions}
#'   \item{\code{nSyl}}{Number of syllables in a word}
#'   \item{\code{stressSylNum}}{The position of the stressed syllable starting from the right edge of the word}
#'   \item{\code{nSylPre}}{Number of pre-tonic syllables}
#'   \item{\code{nOnset.ant}}{Number of onset segments in antepenultimate position}
#'   \item{\code{nOnset.pen}}{Number of onset segments in penultimate position}
#'   \item{\code{nOnset.fin}}{Number of onset segments in final position}
#'   \item{\code{nucleus.ant}}{Number of nuclear segments in antepenultimate position}
#'   \item{\code{nucleus.pen}}{Number of nuclear segments in penultimate position}
#'   \item{\code{nucleus.fin}}{Number of nuclear segments in final position}
#'   \item{\code{nCoda.ant}}{Number of coda segments in antepenultimate position}
#'   \item{\code{nCoda.pen}}{Number of coda segments in penultimate position}
#'   \item{\code{nCoda.fin}}{Number of coda segments in final position}
#'   \item{\code{onset.ant}}{Onset segments in antepenultimate position}
#'   \item{\code{onset.pen}}{Onset segments in penultimate position}
#'   \item{\code{onset.fin}}{Onset segments in final position}
#'   \item{\code{vowel.ant}}{Vowel segments in antepenultimate position}
#'   \item{\code{vowel.pen}}{Vowel segments in penultimate position}
#'   \item{\code{vowel.fin}}{Vowel segments in final position}
#'   \item{\code{coda.ant}}{Coda segments in antepenultimate position}
#'   \item{\code{coda.pen}}{Coda segments in penultimate position}
#'   \item{\code{coda.fin}}{Coda segments in final position}
#'   \item{\code{nOnset.stress}}{Number of onsets in stressed position}
#'   \item{\code{nucleus.stress}}{Number of nuclear segments in stressed position}
#'   \item{\code{nCoda.stress}}{Number of codas in stressed position}
#'   \item{\code{onset.stress}}{Onsets in stressed position}
#'   \item{\code{vowel.stress}}{Vowels in stressed position}
#'   \item{\code{coda.stress}}{Codas in stressed position}
#'   \item{\code{nOnset.alt}}{Number of onsets in alternative position}
#'   \item{\code{nucleus.alt}}{Number of nuclear segments in alternative position}
#'   \item{\code{nCoda.alt}}{Number of codas in alternative position}
#'   \item{\code{onset.alt}}{Onsets in alternative position}
#'   \item{\code{vowel.alt}}{Vowels in alternative position}
#'   \item{\code{coda.alt}}{Codas in alternative position}
#'   \item{\code{POS}}{Part of speech. The lexicon only has nouns and adjectives}
#'   \item{\code{int1}}{Number of segments in V-to-V interval 1 based Steriade (2012). Pre-vocalic consonants are extrametrical in antepenultimate position}
#'   \item{\code{int2}}{Number of segments in V-to-V interval 2 based Steriade (2012). Pre-vocalic consonants are extrametrical in antepenultimate position}
#'   \item{\code{int3}}{Number of segments in V-to-V interval 3 based Steriade (2012). Pre-vocalic consonants are extrametrical in antepenultimate position}
#'   \item{\code{INT1}}{Number of segments in V-to-V interval 1 based Steriade (2012). Pre-vocalic consonants are NOT extrametrical in antepenultimate position}
#'   \item{\code{INT2}}{Number of segments in V-to-V interval 2 based Steriade (2012). Pre-vocalic consonants are NOT extrametrical in antepenultimate position}
#'   \item{\code{INT3}}{Number of segments in V-to-V interval 3 based Steriade (2012). Pre-vocalic consonants are NOT extrametrical in antepenultimate position}
#'   \item{\code{rhyme.ant}}{Whether the rhyme in antepenultimate position is light or heavy}
#'   \item{\code{rhyme.pen}}{Whether the rhyme in penultimate position is light or heavy}
#'   \item{\code{rhyme.fin}}{Whether the rhyme in final position is light or heavy}
#'   \item{\code{weightProfile}}{Weight profile of a word using Ls and Hs}
#'   \item{\code{wordProfile}}{Word profile in Cs and Vs}
#'   \item{\code{finalWordProfile}}{wordProfile for trisyllabic window}
#'   \item{\code{proU}}{Phonemic transcription excluding syllabification and stress}
#'   \item{\code{stemPro}}{proU excluding final phoneme}
#'   \item{\code{antSyl}}{Syllable in antepenultimate position}
#'   \item{\code{penSyl}}{Syllable in penultimate position}
#'   \item{\code{finSyl}}{Syllable in final position}
#'   \item{\code{stress}}{Stress position (numeric), where 1 = final stress and 3 = antepenultimate stress}
#'   \item{\code{stressLoc}}{Stress position}
#'   \item{\code{ID}}{Unique ID column for each row}
#'   \item{\code{bigramProb}}{Bigram probability (log)}
#'   \item{\code{bigramProbStd}}{Standardized bigram probability}
#'   \item{\code{nDensity}}{Neighbourhood density}
#'   \item{\code{nDensityStd}}{Standardized neighbourhood density}
#'   \item{\code{POU}}{Point of uniqueness: the sound, from beginning to end of the word, where it diverges from all other morphologically unrelated words}
#'   \item{\code{POUstd}}{Standardized point of uniqueness}

#' }
#' @source <https://gdgarcia.ca/psl>
#' @author Guilherme D. Garcia (\url{https://gdgarcia.ca})
"psl"

#' Data: stopwords in Portuguese
#'
#' Set of stopwords in Portuguese from the tm package.
#'
#' @format A vector with stopwords in Portuguese
#' @usage data(stopwords_pt)
#' @author Guilherme D. Garcia (\url{https://gdgarcia.ca})
#' @source <https://cran.r-project.org/web/packages/tm/index.html>
"stopwords_pt"

#' Data: stopwords in French
#'
#' Set of stopwords in French from the tm package.
#'
#' @format A vector with stopwords in French
#' @usage data(stopwords_fr)
#' @author Guilherme D. Garcia (\url{https://gdgarcia.ca})
#' @source <https://cran.r-project.org/web/packages/tm/index.html>
"stopwords_fr"

#' Data: stopwords in Spanish
#'
#' Set of stopwords in Spanish from the tm package.
#'
#' @format A vector with stopwords in Spanish
#' @usage data(stopwords_sp)
#' @author Guilherme D. Garcia (\url{https://gdgarcia.ca})
#' @source <https://cran.r-project.org/web/packages/tm/index.html>
"stopwords_sp"

#' Data: comprehensive table with distinctive features
#'
#' Comprehensive dataset with phonemes and their distinctive features.
#' The list is based on PanPhon, by Mortensen et al (2016), whose GitHub can be found
#' at https://github.com/dmort27/panphon
#'
#' @format A data frame with with one IPA symbol per row and one feature per column
#' The (abbreviated) available features are: \code{syl}, \code{son},
#' \code{cons}, \code{cont}, \code{DR}, \code{lat}, \code{nas}, \code{strid},
#' \code{vce}, \code{sg}, \code{cg}, \code{ant}, \code{cor}, \code{distr},
#' \code{lab}, \code{hi}, \code{lo}, \code{back}, \code{round}, \code{vel},
#' \code{tense}, \code{long}, \code{hitone}, \code{hireg}, \code{approx}
#' @usage data(allFeatures)
#' @author Guilherme D. Garcia (\url{https://gdgarcia.ca})
#' @source Adapted from <https://github.com/dmort27/panphon/blob/master/panphon/data/ipa_all.csv>
"allFeatures"

#' Data: hypothetical vowel formants
#'
#' Formant values (F1 and F2) for a 5-vowel system
#'
#' @format A hypothetical dataset with vowel formants
#' @usage data(vowels)
#' @author Guilherme D. Garcia (\url{https://gdgarcia.ca})
#' @source <https://gdgarcia.ca/vowels.html>
"vowels"

#' Data: bigrams in Portuguese
#'
#' Set of bigrams extracted from the Portuguese Stress Lexicon using the ngram package.
#'
#' @format A tibble with bigrams, frequency, and proportions
#' @usage data(bigrams_pt)
#' @author Guilherme D. Garcia (\url{https://gdgarcia.ca})
#' @source <https://gdgarcia.ca/psl>
"bigrams_pt"

#' Data: vowel inventory for French
#'
#' The inventory of vowels in French
#'
#' @format A vector containing the consonants and glides in French
#' @author Guilherme D. Garcia (\url{https://gdgarcia.ca})
#' @usage data(vowels_fr)
"vowels_fr"

#' Data: vowel inventory for Italian
#'
#' The inventory of vowels in Italian
#'
#' @author Guilherme D. Garcia (\url{https://gdgarcia.ca})
#' @format A vector containing the consonants and glides in Italian
#' @usage data(vowels_it)
"vowels_it"

#' Data: vowel inventory for Portuguese
#'
#' The inventory of vowels in Portuguese
#'
#' @author Guilherme D. Garcia (\url{https://gdgarcia.ca})
#' @format A vector containing the consonants and glides in Portuguese
#' @usage data(vowels_pt)
"vowels_pt"

#' Data: vowel inventory for Spanish
#'
#' The inventory of vowels in Spanish
#'
#' @author Guilherme D. Garcia (\url{https://gdgarcia.ca})
#' @format A vector containing the consonants and glides in Spanish
#' @usage data(vowels_sp)
"vowels_sp"

#' Data: consonant inventory for French
#'
#' The inventory of consonants and glides in French
#'
#' @author Guilherme D. Garcia (\url{https://gdgarcia.ca})
#' @format A vector containing the consonants and glides in French
#' @usage data(consonants_fr)
"consonants_fr"

#' Data: consonant inventory for Italian
#'
#' The inventory of consonants and glides in Italian
#'
#' @author Guilherme D. Garcia (\url{https://gdgarcia.ca})
#' @format A vector containing the consonants and glides in Italian
#' @usage data(consonants_it)
"consonants_it"

#' Data: consonant inventory for Portuguese
#'
#' The inventory of consonants and glides in Portuguese
#'
#' @author Guilherme D. Garcia (\url{https://gdgarcia.ca})
#' @format A vector containing the consonants and glides in Portuguese
#' @usage data(consonants_pt)
"consonants_pt"

#' Data: consonant inventory for Spanish
#'
#' The inventory of consonants and glides in Spanish
#' @author Guilherme D. Garcia (\url{https://gdgarcia.ca})
#' @format A vector containing the consonants and glides in Spanish
#' @usage data(consonants_sp)
"consonants_sp"

#' Data: sample tableau for NHG (1)
#'
#' Dummy example to be used in intro to Noisy Harmonic Grammars
#' @author Guilherme D. Garcia (\url{https://gdgarcia.ca})
#' @format A tibble to be used with the \code{nhg()} function
#' @usage data(tableau_nhg_1)
"tableau_nhg_1"

#' Data: sample tableau for NHG (2)
#'
#' Dummy example to be used in intro to Noisy Harmonic Grammars
#' @author Guilherme D. Garcia (\url{https://gdgarcia.ca})
#' @format A tibble to be used with the \code{nhg()} function
#' @usage data(tableau_nhg_2)
"tableau_nhg_2"

#' Data: sample MaxEnt tableau (1)
#'
#' Dummy example to be used in intro to MaxEnt Grammars
#' @author Guilherme D. Garcia (\url{https://gdgarcia.ca})
#' @format A tibble to be used with the \code{maxent()} function
#' @usage data(tableau_nhg_2)
"tableau_maxent_1"


#' Data: contrastive features from Hayes (2009)
#' The complete table of features found in Hayes's 2009
#' Introductory Phonology
#' @author Guilherme D. Garcia
#' @usage data(features_Hayes_2009)
"features_Hayes_2009"
