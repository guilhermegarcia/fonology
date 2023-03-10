% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/IPA_pt_vec.R
\name{ipa_pt_simple}
\alias{ipa_pt_simple}
\title{Vectorized IPA transcriber for Portuguese}
\usage{
ipa_pt_simple(word = c("palavra"))
}
\arguments{
\item{word}{A possible string in Portuguese in its orthographic form}
}
\value{
The phonemic transcription for the string in question
}
\description{
Given a string, the function returns its IPA transcription with stress and syllabification.
Stress is assigned on the basis of the Portuguese Stress Lexicon for existing words, or lexical regularities
and probabilistic distributions in certain cases for hypothetical words
}
\examples{
ipa_pt_simple(word = c("palado", "antedom"));
}
