% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getWeight.R
\name{getWeight}
\alias{getWeight}
\title{Syllable weight labeller}
\usage{
getWeight(word, lg = "Portuguese")
}
\arguments{
\item{word}{The vector with strings of interest using IPA phonemic transcription, already syllabified and stressed}

\item{lg}{Language (defaults to Portuguese)}
}
\value{
The sequence of Ls and Hs based on the word's weight profile given the phonology of the language in question
}
\description{
Labels a given string in Portuguese according to its weight profile using Ls and Hs.
}
\examples{
getWeight(word = c("kom.pu.ta.ˈdoɾ", "ka.ˈloɾ.pe"))
}
