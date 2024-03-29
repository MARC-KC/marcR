% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/javaHelpers.R
\name{java_install}
\alias{java_install}
\alias{java_get_versions}
\title{Install Microsoft or Adoptium built OpenJDK Binaries}
\usage{
java_install(
  installLocation = here::here("JavaEnv"),
  majorVersion = "11",
  source = c("microsoft", "adoptium"),
  os = c("windows", "linux", "macOS"),
  arch = c("x64", "aarch64")
)

java_get_versions(source = c("microsoft", "adoptium"))
}
\arguments{
\item{installLocation}{The location you want to install Java to. Defaults to
here::here("JavaEnv") in your project directory, but could also be stored
to a 'renv' project directory, the user folder like
'$USERPROFILE/AppData/Local'.}

\item{majorVersion}{A major java version to download. Must be one listed by
\code{validJavaVersions()}.}

\item{source}{What binary version do you want to install? Either 'microsoft'
or 'adoptium'.}

\item{os}{One of 'windows', 'linux', or 'macOS'. Currently only 'windows' is
valid.}

\item{arch}{One of 'x64' or 'aarch64'. 'x64' is default.}
}
\value{
Adds the unzipped/untared package to the location specified by
installLocation.
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#maturing}{\figure{lifecycle-maturing.svg}{options: alt='[Maturing]'}}}{\strong{[Maturing]}}

Install a package level version of Java.
}
\section{Functions}{
\itemize{
\item \code{java_install}: Install java to specified location.

\item \code{java_get_versions}: Retrieve the versions of Java available at the download endpoint.
}}

\examples{
\dontrun{
java_install(
    installLocation = file.path(
        normalizePath(Sys.getenv("USERPROFILE"), winslash = "/"),
        'APPDATA', 'Local', "r-OpenJDK"
    ),
    majorVersion = "17", 
    source = "adoptium"
)
}

}
