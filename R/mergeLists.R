#' @title Merge (concatenate) lists elementWise
#' @description 
#' `r lifecycle::badge('stable')`  
#' 
#' Wrapper around mapply to do element-wise concatenation of a list
#' 
#' @param x,y Lists with the same structure and element classes that can be 
#'  concatenated with `c`.
#' 
#' @return Merged list with the same structure as x and y.
#' 
#' @section Creation notes: First created on 2021-01-11 while working in the 
#'  BuildingFootprints DataDevelopment project.
#' 
#' @examples
#' \dontrun{
#' xyz <- tibble::tribble(
#'     ~x,            ~y,           ~z,           ~xyz,
#'     11L,           integer(0),   integer(0),   11L,
#'     120L,          220L,         320L,         c(120L, 220L, 320L),
#'     c(13L, 14L),   integer(0),   33L,          c(13L, 14L, 33L),
#'     integer(0),    integer(0),   integer(0),   integer(0),
#'     15L,           integer(0),   integer(0),   15L,
#'     
#'     integer(0),    21L,          integer(0),   21L,
#'     integer(0),    22L,          integer(0),   22L,
#'     integer(0),    23L,          integer(0),   23L,
#'     integer(0),    24L,          integer(0),   24L,
#'     integer(0),    25L,          integer(0),   25L,
#'     
#'     integer(0),    integer(0),   31L,          31L,
#'     integer(0),    integer(0),   32L,          32L,
#'     integer(0),    integer(0),   33L,          33L,
#'     integer(0),    integer(0),   34L,          34L,
#'     integer(0),    integer(0),   35L,          35L
#'     )
#' 
#' mergeList(xyz[['x']],xyz[['y']])
#' mergeList(xyz[['x']],xyz[['z']])
#' Reduce(mergeList,xyz[c('x', 'y', 'z')])
#' identical(Reduce(mergeList,xyz[c('x', 'y', 'z')]), xyz[['xyz']])
#' }
#' @export
mergeList <- function(x,y) {
  mapply(c, x, y, SIMPLIFY=FALSE)
}