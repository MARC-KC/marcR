#' @title Indent lines
#'
#' @description
#' Splits a string by newlines and adds a chosen number of tabs to each line.
#'
#' @details
#' 
#'
#' @param string String you want to add tabs to
#' @param tabs Number of tabs you want to indent each line

#'
#' @return A \code{glue} character string
#'
#' @section Creation notes: First created in 2020-Oct while for building dynamic
#'   SQL statements.
#'   
#' @author Jacob Peterson
#'
#' @examples
#' \dontrun{
#' library(glue)
#' library(stringr)
#'
#' tableRows <- glue::glue("
#' <tr class='rowSeperator'>
#'     <td>Test</td>
#' </tr>
#' ")
#'
#' glueIndentLines(tableRows, 2)
#' }

#' @export
glueIndentLines <- function(string, tabs = 1) {
  tabString <- rep("    ", tabs) %>% paste0(collapse = "")
  paste0(tabString, stringr::str_split(string, "\\n")[[1]] ) %>% glue::glue_collapse(sep="\n")
}