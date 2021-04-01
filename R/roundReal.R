

#' @title Round all doubles in a data.frame
#'
#' @description Rounds all doubles (skipping dates and POSIX) in a data.frame to
#'   a specified number of digits. Created to use with SQL tables that sometimes
#'   have small number rounding errors.
#'  
#' @param df Input data.frame to mutate
#' @param digits Number of digits to round to. Default is 5.
#'   
#' @return A data.frame/tibble
#'   
#' @examples 
#' 
#' \dontrun{
#' iris %>% roundReal(0)
#' }
#'  
#' @export   
roundReal <- function(df, digits = 5) {
  out <- dplyr::mutate(df, dplyr::across(where(~is.double(.x) && 
                                          !lubridate::is.POSIXt(.x) && 
                                          !lubridate::is.Date(.x)), 
                                  ~round(.x, digits = digits)))
  
  out
}
