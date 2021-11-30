
#' Helpers for group_by |> mutate(leadOrLag)
#' 
#' @description 
#' `r lifecycle::badge('stable')`  
#' 
#' Cuts down on typing to preform relatively common functions.
#' 
#' @param .data data.frame/tibble
#' @param ... group_by fields past to group_by
#' @param lagCol,leadCol what column in .data should be lagged or lead
#' @param newCol name of column that the mutation creates
#' @param n Positive integer of length 1, giving the number of positions to lead or lag by
#' @param order_by Override the default ordering to use another vector or column
#' @param default Value used for non-existant rows. Defaults to NA.
#' @param doUngroup Ungroup the data? TRUE/FALSE. Default TRUE
#' 
#' @return tibble
#' 
#' @examples 
#' \dontrun{
#' 
#' df <- tibble::tribble(
#'   ~x,  ~y,  ~z,
#'   'A',  3,  90,
#'   'B',  3,  150,
#'   'B',  2,  123,
#'   'A',  1,  125,
#'   'C',  1,  111,
#'   'C',  2,  133,
#'   'B',  1,  121,
#'   'A',  2,  108,
#'   'C',  3,  125
#' )
#' 
#' groupby_lag(df, x, lagCol = z, newCol = "test", n = 1, order_by = y) %>%
#'   dplyr::arrange(x, y)
#' groupby_lag(df, x, lagCol = z, newCol = "test", n = 1) %>%
#'   dplyr::arrange(x)
#' groupby_lead(df, x, leadCol = z, newCol = "test", n = 1, order_by = y) %>%
#'   dplyr::arrange(x, y)
#' groupby_lead(df, x, leadCol = z, newCol = "test", n = 1) %>%
#'   dplyr::arrange(x)
#' 
#' }
#' 
#' @rdname groupby_lagLead
#' @export
groupby_lag <- function(.data, ..., lagCol, newCol, n = 1L, order_by = NULL, default = NA, doUngroup = TRUE) {
  lagCol <- rlang::ensym(lagCol)
  newCol <- rlang::ensym(newCol)
  order_by <- tryCatch(rlang::ensym(order_by), error = function(e) {return(NULL)})
  
  
  #Create group_by
  data <- dplyr::group_by(.data, ...)
  
  #Create Lagged Column
  if (!is.null(order_by)) {
    data <- dplyr::mutate(data, !!newCol := dplyr::lag(!!lagCol, n = n, default = default, order_by = !!order_by))
  } else {
    data <- dplyr::mutate(data, !!newCol := dplyr::lag(!!lagCol, n = n, default = default, order_by = order_by))
  }
  
  #Ungroup
  if (doUngroup) {
    data <- dplyr::ungroup(data)
  }
  
  data
}

#' @rdname groupby_lagLead
#' @export
groupby_lead <- function(.data, ..., leadCol, newCol, n = 1L, order_by = NULL, default = NA, doUngroup = TRUE) {
  leadCol <- rlang::ensym(leadCol)
  newCol <- rlang::ensym(newCol)
  order_by <- tryCatch(rlang::ensym(order_by), error = function(e) {return(NULL)})
  
  
  #Create group_by
  data <- dplyr::group_by(.data, ...)
  
  #Create Lead Column
  if (!is.null(order_by)) {
    data <- dplyr::mutate(data, !!newCol := dplyr::lead(!!leadCol, n = n, default = default, order_by = !!order_by))
  } else {
    data <- dplyr::mutate(data, !!newCol := dplyr::lead(!!leadCol, n = n, default = default, order_by = order_by))
  }
  
  #Ungroup
  if (doUngroup) {
    data <- dplyr::ungroup(data)
  }
  
  data
}
