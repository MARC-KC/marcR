#' @title {binman}-like Pre download Github assets
#' @description 
#' `r lifecycle::badge('stable')`  
#' 
#' This is a wrapper around \code{binman::predl_github_assets()}
#'   that adds the parameter \code{dlPath} that allows the user to specify the
#'   binman download path. This was built to allow custom binman install
#'   directories for project specific environments.
#'
#' @param url A url giving the github asset JSON for a project. As an example
#'   https://github.com/mozilla/geckodriver/releases the geckodriver project has
#'   an asset JSON available at
#'   https://api.github.com/repos/mozilla/geckodriver/releases
#' @param platform A character vector of platform names
#' @param history The maximum number of files to get for a platform
#' @param appname Name of the app
#' @param platformregex A filter for platforms. Defaults to the platform
#' @param versionregex A regex for retrieving the version.
#' @param dlPath Custom download path for the binman folder. Defaults to NULL,
#'   where it will return the same table as \code{binman::predl_github_assets()}
#'   with an unmodified download path.
#'
#' @return A named list of data.frames. The name indicates the platform. The
#'   data.frame should contain the version, url and file to be processed. Used
#'   as input for \code{\link{download_files}} or an equivalent.
#' @export
#'
#' @examples
#' \dontrun{
#' dllist <- binman_predl_github_assets(
#'     url = "https://api.github.com/repos/mozilla/geckodriver/releases",
#'     platform = c("win64"),
#'     history = 1L,
#'     appname = "geckodriver",
#'     versionregex = c("^v", ""),
#'     dlPath = here::here()
#'  )
#' dllist
#' }
binman_predl_github_assets <- function(url, platform, history = 3L, appname, platformregex = platform, versionregex = c("", ""), dlPath = NULL) {
  
  #Run main binma function
  dllist <- binman::predl_github_assets(
    url = url,
    platform = platform,
    history = history,
    appname = appname,
    platformregex = platformregex,
    versionregex = versionregex
  )
  
  if (is.null(dlPath) | !dir.exists(dlPath)) {
    return(dllist)
  }
  
  
  dllist <- dllist %>% purrr::map(~{
    .x %>% dplyr::mutate(dir = dir %>% purrr::map(~{
      .x %>% normalizePath(path = ., winslash = "/", mustWork = FALSE) %>% 
        stringr::str_extract("(?<=/binman/).*$") %>% 
        file.path(dlPath, "binman", .)
    }), exists = purrr::map_lgl(dir, ~dir.exists(dirname(.x))) %>% 
      `names<-`(NULL))
  })
  
  return(dllist)
}





#' @title Get suggested binman path
#' @description 
#' `r lifecycle::badge('stable')`  
#' 
#' Get the default base path for a binman download folder. If your
#'   working directory is in a project with a 'renv' folder, it will suggest
#'   that as the install location. Otherwise the path specified by
#'   \code{rappdirs::user_data_dir()} will be used.
#'
#' @param path Path to check. Defaults to result of \code{here::here()}
#'
#' @return Returns a file path
#'
#' @examples
#' \dontrun{
#' binman_path_base()
#' }
#'
#' @export
binman_path_base <- function(path = here::here()) {
  
  if (is_renv_used(path)) {
    return(file.path(path, 'renv'))
  } else {
    return(normalizePath(rappdirs::user_data_dir(), winslash = "/", mustWork = FALSE))
  }
  
}







