% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/age_conversion.R
\name{monthsAge}
\alias{monthsAge}
\title{Age conversion}
\usage{
monthsAge(age, sep = ";")
}
\arguments{
\item{age}{Age in yy;mm format}

\item{sep}{Separator for age. Default is ";"}
}
\value{
Age in months
}
\description{
Calculates age in months based on yy;mm format, commonly used in language acquisition studies.
}
\examples{
monthsAge(age = "02;06")
monthsAge(age = "05:03", sep = ":")
}
