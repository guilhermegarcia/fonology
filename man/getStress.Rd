% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/which_stress_pt.R
\name{getStress}
\alias{getStress}
\title{Stress labeller}
\usage{
getStress(word = c("kom.pu.ta.ˈdoɾ"), stress = "ˈ", syl = FALSE)
}
\arguments{
\item{word}{The string of interest using IPA phonemic transcription, already syllabified and stressed. Syllable boundaries are always assumed to be indicated by a period.}

\item{stress}{The symbol used to mark stress}

\item{syl}{Whether the function should return the stressed syllable instead of the stress position (default is \code{FALSE})}
}
\value{
The primary stress position
}
\description{
Labels a given stressed string as follows: Final, Penult, Antepenult.
Alternatively, extracts stressed syllable from input.
}
\examples{
getStress(word = "kom.pu.ta.ˈdoɾ", stress = "ˈ")
getStress(word = "kom.pu.ta.ˈdoɾ", stress = "ˈ", syl = TRUE)
}
