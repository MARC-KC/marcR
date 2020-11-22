.onLoad <- function(libname, pkgname){
  if (!any(grepl("esri2sf", installed.packages()[,"Package"]))) cat(crayon::green("Install the package 'esri2sf' from github with: `devtools::install_github('yonghah/esri2sf')`\n"))
}