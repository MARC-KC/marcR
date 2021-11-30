#' Helper for group_by |> mutate(rankID) |> filter(rankID)
#' 
#' @description 
#' `r lifecycle::badge('stable')`  
#' 
#' @param .data data.frame/tibble
#' @param ... group_by fields passed to group_by
#' @param rankby what column to rank by as character
#' @param rankDesc descending rank? TRUE/FALSE
#' @param filterIDs what rankID's to filter out. Default NULL
#' @param removeRankCol Remove rank column? TRUE/FALSE. Only used if filtering
#' @param doUngroup Ungroup the data? TRUE/FALSE
#' 
#' @return tibble
#' 
#' @details Uses a dense rank to mimic Microsoft T-SQL rank
#' 
#' @section Creation notes: First created in 2020-12-11 while working in the
#'   MARC-KC/CovidDataEntry Repository
#' 
#' @examples
#' \dontrun{
#' groupby_rank(iris, Species, rankby = "Sepal.Length", rankDesc = TRUE, filterIDs = 1)
#' }
#' @export
groupby_rank <- function(.data, ..., rankby, rankDesc = TRUE, filterIDs = NULL, removeRankCol = TRUE, doUngroup = TRUE) {
  rankby <- rlang::ensym(rankby)
  
  #Create group_by
  data <- dplyr::group_by(.data, ...)
  
  #Create rank ID
  if (rankDesc) {
    data <- dplyr::mutate(data, rankID = dplyr::dense_rank(dplyr::desc(!!rankby)))
  } else {
    data <- dplyr::mutate(data, rankID = dplyr::dense_rank(!!rankby))
  }
  
  #filter by rankID
  if (!is.null(filterIDs)) {
    data <- dplyr::filter(data, rankID %in% filterIDs)
    
    #Remove rankID?
    if (removeRankCol) {
      data <- dplyr::select(data, -rankID)
    }
  }
  
  #Ungroup
  if (doUngroup) {
    data <- dplyr::ungroup(data)
  }
  
  data
}