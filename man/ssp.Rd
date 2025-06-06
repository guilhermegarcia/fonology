% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ssp.R
\name{ssp}
\alias{ssp}
\title{Sonority sequencing principle}
\usage{
ssp(demi = "", d = 1)
}
\arguments{
\item{demi}{A demi syllable; see function demi()}

\item{d}{Whether the function should return first \code{(1)} or second \code{(2)} demisyllable}
}
\value{
Either \code{1}, indicating that the sequence respects the SSP, or \code{0}
}
\description{
Given a demisyllable, the function returns \code{1} if the demisyllable respects
the sonority sequencing principle (SSP) and \code{0} otherwise.
}
\examples{
ssp(demi = "tra", d = 1)
}
