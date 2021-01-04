#' @title Download Data from MARCDataAPI
#'
#' @description Creates a GET request for a MARCDataAPI URL and returns the result as a tibble
#'
#'
#' @param url A url to a MARCDataAPI.
#' @param query A list of key-values for queries to append to the GET request URL. Currently untested.
#'
#' @return OBDC connection object returned from DBI::dbConnect()
#' 
#' @details See https://rpubs.com/ankc/480665 for how query should work theoretically.
#'
#' @section Creation notes: First created in 2020-12-30 for easily accessing
#'   data distributed by the MARCData API.
#'
#' @author Jacob Peterson
#'
#' @examples
#' \dontrun{
#' df <- MARCDataAPI_read('https://gis2.marc2.org/MARCDataAPI/api/covidcasedeathtest') %>% 
#'   dplyr::mutate(Date = as.Date(Date),
#'                 LastUpdated = lubridate::as_datetime(LastUpdated),
#'                 LastUpdated = lubridate::with_tz(LastUpdated, "America/Chicago")) 
#' df
#' }
#' @export
MARCDataAPI_read <- function(url, query = NULL) {
  df <- httr::GET(url, query = query) %>% 
    httr::content(type = 'application/json') %>% 
    purrr::map_dfr(~{
      .x <- purrr::map(.x, ~if (is.null(.x)) NA else .x)
      tibble::as_tibble(.x) 
    })
  return(df)
}