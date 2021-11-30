
#' @title Coalesce Duplicate Fields from a Join
#' 
#' @description 
#' `r lifecycle::badge('stable')`  
#' 
#' Coalesces columns by suffix created by joins. Useful after a full join.
#' 
#' @param df the joined data.frame
#' @param suffix suffix used to diferentiate the columns needing coalesced. Defaults to c(".x", ".y")
#' @param showMessage TRUE/FALSE on whether to print message. Default TRUE.
#' 
#' @return df with the join suffixed columns coalesced
#' 
#' @examples 
#' \dontrun{
#' #Download Case, Death, Test Data
#' cdtData <- MARCDataAPI_read('https://gis2.marc2.org/MARCDataAPI/api/covidcasedeathtest') %>%
#'   dplyr::mutate(Date = as.Date(Date),
#'                 LastUpdated = lubridate::as_datetime(LastUpdated),
#'                 LastUpdated = lubridate::with_tz(LastUpdated, "America/Chicago"))
#' 
#' #Download the Hospital Data
#' hospData <- marcR::MARCDataAPI_read('https://gis2.marc2.org/MARCDataAPI/api/covidhospital') %>%
#'     dplyr::mutate(Date = as.Date(Date))
#' 
#' cdtHospSumData <- dplyr::full_join(cdtData, hospData, by = c("GeoID", "Date")) %>% coalesceJoin()
#' }
#' @export
coalesceJoin <- function(df, suffix = c(".x", ".y"), showMessage = TRUE) {
  
  suffixNames <- purrr::map(suffix, ~names(df)[endsWith(names(df),.x)]) %>% unlist()
  coalesceNames <- purrr::map(suffix[1], ~names(df)[endsWith(names(df),.x)]) %>% unlist() %>% stringr::str_remove(suffix[1])
  
  
  coalesced <- purrr::map_dfc(coalesceNames, ~dplyr::coalesce(
    df[[paste0(.x, suffix[1])]],
    df[[paste0(.x, suffix[2])]]
  ) %>% tibble::tibble() %>% `names<-`(.x))
  
  otherData <- dplyr::select(df, -all_of(suffixNames))
  
  if (showMessage) {
    message(crayon::yellow(glue::glue('Coalesced the following columns: {glue::glue_collapse(coalesceNames, sep = ", ")}')))
  }
  
  return(dplyr::bind_cols(coalesced, otherData))
  
}
