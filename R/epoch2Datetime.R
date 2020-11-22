#' @title Change Epoch Time to Datetime
#'
#' @description Converts epoch time to a POSIXct/datetime. This is a common task
#' when reading in data with dates from an ESRI REST API.
#'
#' @param epochDate numeric vector with the epoch date
#' @param milliseconds Should you complete millisecond conversion? Default is
#'   TRUE
#'
#' @return Vector of class POSIXct
#'
#'
#' @section Creation notes: First created in 2020-Sept in conjunction with my
#'   pull request to \code{esri2sf} to add the \code{esri2df} function
#'
#' @author Jacob Peterson
#'
#' @examples
#' \dontrun{
#' library(esri2sf)
#'
#' esriData <- esri2sf::esri2df(url = "https://gis2.marc2.org/arcgis/rest/services/HumanServices/COVIDv2/MapServer/5")
#' class(esriData$Date)
#'
#' class(epoch2Datetime(esriData$Date))
#' }

#' @export
epoch2Datetime <- function(epochDate, milliseconds = TRUE) {
  if (milliseconds) {
    dateConversion <- as.numeric(epochDate)/1000
  } else {
    dateConversion <- as.numeric(epochDate)
  }
  out <- as.POSIXct(dateConversion, origin="1970-01-01")
  return(out)
}