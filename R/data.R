#' Portuguese Stress Lexicon - simplified
#'
#' Comprehensive list of non-verbs in Portuguese and their phonemic transcription.
#' This data set is used by different functions in Fonology. Unlike the original PSL data base,
#' `pt_lex` uses slightly different transcription conventions.
#'
#' @format ## `pt_lex`
#' A data frame with 154,610 rows and 2 columns:
#' \describe{
#'   \item{word}{A word in its orthographic form}
#'   \item{pro}{The phonemic transcription of a word}
#' }
#' @source <https://gdgarcia.ca/psl>
"pt_lex"

#' Portuguese Stress Lexicon
#'
#' Comprehensive list of non-verbs in Portuguese.
#' The lexicon is coded for a number of phonological variables.
#'
#' @format ## `psl`
#' A data frame with 154,610 rows and 62 columns:
#' \describe{
#'   \item{word}{A word in its orthographic form}
#'   \item{pro}{The phonemic transcription of a word. This uses slightly different conventions than those in Fonology transcriptions}
#'   \item{nSyl}{Number of syllables in a word}
#'   \item{stressSylNum}{The position of the stressed syllable starting from the right edge of the word}
#'   \item{nSylPre}{Number of pre-tonic syllables}
#'   \item{nOnset.xxx}{Number of onset segments in position xxx}
#'   \item{nucleus.xxx}{Number of nuclear segments in position xxx}
#'   \item{nCoda.xxx}{Number of coda segments in position xxx}
#'   \item{onset.xxx}{Onset segments in position xxx}
#'   \item{vowel.xxx}{Vowel segments in position xxx}
#'   \item{coda.xxx}{Coda segments in position xxx}
#'   \item{onset.xxx}{Number of syllables in a word}
#'   \item{nOnset.stress}{Number of onsets in stressed position}
#'   \item{nucleus.stress}{Number of nuclear segments in stressed position}
#'   \item{nCoda.stress}{Number of codas in stressed position}
#'   \item{onset.stress}{Onsets in stressed position}
#'   \item{vowel.stress}{Vowels in stressed position}
#'   \item{coda.stress}{Codas in stressed position}
#'   \item{nOnset.alt}{Number of onsets in alternative position}
#'   \item{nucleus.alt}{Number of nuclear segments in alternative position}
#'   \item{nCoda.alt}{Number of codas in alternative position}
#'   \item{onset.alt}{Onsets in alternative position}
#'   \item{vowel.alt}{Vowels in alternative position}
#'   \item{coda.alt}{Codas in alternative position}
#'   \item{POS}{Part of speech. The lexicon only has nouns and adjectives}
#'   \item{intx}{Number of segments in V-to-V interval x based Steriade (2012). Pre-vocalic consonants are extrametrical in antepenultimate position}
#'   \item{INTx}{Number of segments in V-to-V interval x based Steriade (2012). Pre-vocalic consonants are NOT extrametrical in antepenultimate position}
#'   \item{rhyme.xxx}{Whether the rhyme in position x is light or heavy}
#'   \item{weightProfile}{Weight profile of a word using Ls and Hs}
#'   \item{wordProfile}{Word profile in Cs and Vs}
#'   \item{finalWordProfile}{wordProfile for trisyllabic window}
#'   \item{proU}{Phonemic transcription excluding syllabification and stress}
#'   \item{stemPro}{proU excluding final phoneme}
#'   \item{xxxSyl}{Syllable in xxx position}
#'   \item{stress}{Stress position (numeric), where 1 = final stress and 3 = antepenultimate stress}
#'   \item{stressLoc}{Stress position}
#'   \item{ID}{Unique ID column for each row}
#'   \item{bigramProb}{Bigram probability (log)}
#'   \item{bigramProbStd}{Standardized bigram probability}
#'   \item{nDensity}{Neighbourhood density}
#'   \item{nDensityStd}{Standardized neighbourhood density}
#'   \item{POU}{Point of uniqueness: the sound, from beginning to end of the word, where it diverges from all other morphologically unrelated words}
#'   \item{POUstd}{Standardized point of uniqueness}

#' }
#' @source <https://gdgarcia.ca/psl>
"psl"

#' Stopwords in Portuguese
#'
#' Set of stopwords in Portuguese from the tm package.
#'
#' @format ## `stopwords_pt`
#' A vector with stopwords in Portuguese
#' @source <https://cran.r-project.org/web/packages/tm/index.html>
"stopwords_pt"

#' Distinctive features
#'
#' Comprehensive dataset with phonemes and their distinctive features.
#' The list is based on PanPhon, by Mortensen et al (2016), whose GitHub can be found
#' at https://github.com/dmort27/panphon
#'
#' @format ## `allFeatures`
#' A data frame with with one IPA symbol per row and one feature per column
#' @source <https://github.com/dmort27/panphon/blob/master/panphon/data/ipa_all.csv>
"allFeatures"

