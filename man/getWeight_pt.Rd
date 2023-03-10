% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/weight_pt.R
\name{getWeight_pt}
\alias{getWeight_pt}
\title{Syllable weight labeller}
\usage{
getWeight_pt(word = "kom.pu.ta.ˈdoɾ")
}
\arguments{
\item{word}{The string of interest using IPA phonemic transcription, already syllabified and stressed}
}
\value{
The sequence of Ls and Hs based on the word's weight profile given the phonology of Portuguese
}
\description{
Labels a given string in Portuguese according to its weight profile using Ls and Hs.
Note that, although simple plural -s is taken into account, the function assumes the string provided is monomorphemic.
For a simplified but vectorized version of the function, use getWeight_pt_simple().
}
\examples{
getWeight_pt(word = "kom.pu.ta.ˈdoɾ");
}
