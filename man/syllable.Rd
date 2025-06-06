% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/syllabic_constituents.R
\name{syllable}
\alias{syllable}
\title{Syllable constituent extractor}
\usage{
syllable(
  syl = "",
  const = "nucleus",
  glides_as_onsets = FALSE,
  glides_as_codas = FALSE
)
}
\arguments{
\item{syl}{The syllable of interest using IPA phonemic transcription}

\item{const}{The constituent of interest (defaults to nucleus)}

\item{glides_as_onsets}{Whether glides are to be interpreted as onsets (defaults to \code{FALSE})}

\item{glides_as_codas}{Whether glides are to be interpreted as codas (defaults to \code{FALSE})}
}
\value{
The constituent of interest or \code{NA} if said constituent doesn't exist for the syllable in question
}
\description{
Returns onset, nucleus, or coda for a given syllable.
}
\examples{
syllable(syl = "kom", const = "nucleus")
}
