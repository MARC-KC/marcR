#' @title sumNA
#' @description Wrapper around sum that defaults na.rm to TRUE
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
#' }
#' @export
sumNA <- function(vec) {
  if (all(is.na(vec))) {
    if (class(vec) == "integer") {
      return(NA_integer_)
    } else if (class(vec) %in% c("numeric", "double")) {
      return(NA_real_)
    }
  } else {
    return(base::sum(vec, na.rm = TRUE))
  }
}