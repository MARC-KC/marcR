% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/roundReal.R
\name{roundReal}
\alias{roundReal}
\title{Round all doubles in a data.frame}
\usage{
roundReal(df, digits = 5)
}
\arguments{
\item{df}{Input data.frame to mutate}

\item{digits}{Number of digits to round to. Default is 5.}
}
\value{
A data.frame/tibble
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#stable}{\figure{lifecycle-stable.svg}{options: alt='[Stable]'}}}{\strong{[Stable]}}

Rounds all doubles (skipping dates and POSIX) in a data.frame to
a specified number of digits. Created to use with SQL tables that sometimes
have small number rounding errors.
}
\examples{

\dontrun{
iris \%>\% roundReal(0)
}
 
}
