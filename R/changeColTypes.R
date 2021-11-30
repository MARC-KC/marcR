#' @title Change Column Types
#' 
#' @description 
#' `r lifecycle::badge('stable')`  
#' 
#' Changes column types based on vectors of column names and readr types
#' 
#' @param df data.frame/tibble
#' @param cols Character vector of columns in df to change
#' @param types Character vector of new column types (readr-like)
#' 
#' @return A data.frame/tibble
#' 
#' @section Creation notes: First created in 2020-12-11 while working in the
#'   MARC-KC/CovidDataEntry Repository
#' 
#' @examples
#' \dontrun{
#' class(iris[['Species']])
#' 
#' irisNew <- changeColType(iris, cols = c("Species"), types=c('c'))
#' class(irisNew[['Species']])
#' }
#' 
#' @export
changeColType <- function(df, cols, types) {
  
  #Add check to make sure all cols are in df
  if (!all(cols %in% names(df))) stop("Not all specified column names are in df")
  
  #Add check to make sure length(cols) == length(types)
  if (length(cols) != length(types)) {
    if(length(types) == 1) {
      types <- rep(types, times=length(cols))
    } else {
      stop("length(types) must either be 1 or equal to length(cols).")
    }
  }
  
  #Do type casting (in future may add additional variable for tryFormats for date casting)
  for (i in seq_along(cols)) {
    
    if (types[i] == 'D') {
      df[[cols[i]]] <- as.Date(df[[cols[i]]])
    } else if (types[i] == 'c') {
      df[[cols[i]]] <- as.character(df[[cols[i]]])
    } else if (types[i] == 'i') {
      df[[cols[i]]] <- as.integer(df[[cols[i]]])
    } else if (types[i] == 'l') {
      df[[cols[i]]] <- as.logical(df[[cols[i]]])
    } else if (types[i] == 'd') {
      df[[cols[i]]] <- as.double(df[[cols[i]]])
    } else if (types[i] == 'f') {
      df[[cols[i]]] <- as.factor(df[[cols[i]]])
    } else if (types[i] == 'T') {
      df[[cols[i]]] <- as.POSIXct(df[[cols[i]]])
    } else {
      stop(paste0("Type '", types[i], "' not recognized."))
    }
    
  } 
  
  return(df)
  
}