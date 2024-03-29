% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/DBIhelpers.R
\name{DBI_getColNames}
\alias{DBI_getColNames}
\title{Retrieve Column Names from an Microsoft SQL Connection}
\usage{
DBI_getColNames(conn, schema, tableName)
}
\arguments{
\item{conn}{A \code{\link[DBI:DBIConnection-class]{DBIConnection}} object, as
returned by \code{\link[DBI:dbConnect]{dbConnect()}}.}

\item{schema}{Name of schema that table is under. character vector of length
1.}

\item{tableName}{Name of table. character vector of length 1.}
}
\value{
Character vector of column names in specified table.
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#stable}{\figure{lifecycle-stable.svg}{options: alt='[Stable]'}}}{\strong{[Stable]}}
}
\section{Creation notes}{
 First created on 2020-11-03 with the script
OSMdownloadAndSFtoSQLInterface.R in the MARC-KC/HelpLibrary Repository
}

\examples{
\dontrun{
library(DBI)

con <- connectODBC("<servername>.<databaseName>.<schemaName>")

tableColNames <- DBI_getColNames(con, "<schemaName>", "<tableName>")
}
}
\author{
Jacob Peterson
}
