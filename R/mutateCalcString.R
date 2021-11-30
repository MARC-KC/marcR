#' @title mutateCalcString
#' @description 
#' `r lifecycle::badge('maturing')`  
#' 
#' Allows a mutation given a string for the calculation side of the mutation
#' 
#' @param df data.frame/tibble to change
#' @param mutateName name of the column you want to change or create with the mutation
#' @param mutateCalc calculation (as a string) to be used for the mutation
#' 
#' @return data.frame/tibble with mutated columns
#' 
#' @section Creation notes: First created in 2020-11 while working in the
#'   MARC-KC/COVID-10_MARC Repository
#' 
#' @examples
#' \dontrun{
#' mutateCalcString(iris, "sumLength", "Sepal.Length + Petal.Length")
#' }
#' @export
mutateCalcString <- function(df, mutateName, mutateCalc) {
  mutateString = glue::glue("df${mutateName} = with(df, ({mutateCalc}))")
  for (i in 1:length(mutateString)) {
    eval(parse(text=mutateString[i]))
  }
  return(df)
} 