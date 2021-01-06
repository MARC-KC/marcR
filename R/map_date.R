#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# FNs - map_date and map2_date - Wrappers for purrr::map to output dates ####
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#' @title  map_date and map2_date
#' 
#' @description Wrappers for purrr::map and purrr::map2 to output dates
#' 
#' @param .x,.y list or atomic vector.
#' @param .f A function, formula, or vector (not necessarily atomic).
#'
#'   If a __function__, it is used as is.
#'
#'   If a __formula__, e.g. `~ .x + 2`, it is converted to a function. There
#'   are three ways to refer to the arguments:
#'
#'   * For a single argument function, use `.`
#'   * For a two argument function, use `.x` and `.y`
#'   * For more arguments, use `..1`, `..2`, `..3` etc
#'
#'   This syntax allows you to create very compact anonymous
#'   functions. Note that formula functions conceptually take dots
#'   (that's why you can use `..1` etc). They silently ignore
#'   additional arguments that are not used in the formula expression.
#'
#'   If __character vector__, __numeric vector__, or __list__, it is
#'   converted to an extractor function. Character vectors index by
#'   name and numeric vectors index by position; use a list to index
#'   by position and name at different levels. If a component is not
#'   present, the value of `.default` will be returned.
#' @param ... Additional arguments passed on to methods.
#' 
#' @return  Returns a vector of dates 
#' @examples
#' \dontrun{
#' #Need to add, but very similar to any of the other purrr::map interfaces
#' }
#' @rdname map_date
#' @export
map_date <- function(.x, .f, ...) {
  purrr::map(.x, .f = .f) %>% unlist() %>% as.Date(origin="1970-01-01")
}

#' @rdname map_date
#' @export
map2_date <- function(.x, .y, .f, ...) {
  purrr::map2(.x, .y, .f = .f) %>% unlist() %>% as.Date(origin="1970-01-01")
}
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++