#' Changes column names based on vectors containing old and new colNames
#' 
#' @param df data.frame/tibble
#' @param currentNames Character vector of current column names
#' @param newNames Character vector of new column names
#' 
#' @return A tibble
#' 
#' @section Creation notes: First created in 2020-12-11 while working in the
#'   MARC-KC/CovidDataEntry Repository
#' 
#' @examples 
#' \dontrun{
#' names(iris)
#' names(renameByPairs(iris, 
#'                     currentNames = c("Petal.Length", "Petal.Width"),
#'                     newNames = c("pl", "pw")))
#' }
#' 
#' 
#' @export
renameByPairs <- function(df, currentNames, newNames) {
  keyValue <- `names<-`(currentNames, newNames)
  df <- dplyr::rename(df, all_of(keyValue))
  return(df)
}