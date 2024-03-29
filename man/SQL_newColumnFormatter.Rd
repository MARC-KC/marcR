% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SQLhelpers.R
\name{SQL_newColumnFormatter}
\alias{SQL_newColumnFormatter}
\title{SQL_newColumnFormatter}
\usage{
SQL_newColumnFormatter(df = NULL, types = NULL, charLength, colNames = NULL)
}
\arguments{
\item{df}{data.frame/tibble. Optional. Must have df or types specified.}

\item{types}{Character vector of new column types (readr-like). Optional.
Must have df or types specified.}

\item{charLength}{Vector of lengths used for character (NVARCHAR) types. If
using the df argument, it will also accept a named vector of lengths for
each column name with a character type.}

\item{colNames}{Character vector of columns names to include in database
table. Ignored if using the df argument.}
}
\value{
A string containing partial SQL query for column definitions.
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#stable}{\figure{lifecycle-stable.svg}{options: alt='[Stable]'}}}{\strong{[Stable]}}

Creates the column definition part of a SQL query. Used as a
helper for \code{SQL_createTable} and \code{SQL_addColumns}
}
