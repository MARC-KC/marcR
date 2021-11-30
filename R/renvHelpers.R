#' @title Is {renv} used in current directory/project?
#' @description 
#' `r lifecycle::badge('experimental')`  
#' 
#' Check if your current workspace is in a project with renv
#'   enabled. This is kind of a hack and just looks for a renv folder, but it
#'   should work.
#'
#' @param path Path to check. Defaults to result of \code{here::here()}
#'
#' @return Returns a TRUE/FALSE
#'
#' @examples
#' \dontrun{
#' is_renv_used()
#' }
#'
#' @export
is_renv_used <- function(path = here::here()) {
  has_renv_dir <- any(stringr::str_detect(dir(path, recursive = FALSE), "renv"))
  
  is_home <- identical(normalizePath(path, winslash = "/", mustWork = FALSE), path.expand('~'))
  
  return(has_renv_dir & !is_home)
  
}
