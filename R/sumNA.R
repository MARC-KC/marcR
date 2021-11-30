#' @title sumNA
#' @description 
#' `r lifecycle::badge('stable')`  
#' 
#' Wrapper around sum that defaults na.rm to TRUE
#' 
#' @param vec numeric, double or integer vector
#' 
#' @return Sum of input vector
#' 
#' @section Creation notes: First created in 2020-11 while working in the
#'   MARC-KC/COVID-10_MARC Repository
#' 
#' @examples
#' \dontrun{
#' sumNA(iris[['Sepal.Length']])
#' sumNA(c(NA, TRUE, FALSE, TRUE))
#' }
#' @export
sumNA <- function(vec) {
  if (all(is.na(vec))) {
    if (class(vec) %in% c("numeric", "double")) {
      return(NA_real_)
    } else {
      return(NA_integer_)
    }
  } else {
    return(base::sum(vec, na.rm = TRUE))
  }
}