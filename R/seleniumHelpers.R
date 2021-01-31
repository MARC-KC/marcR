#'
#' @title Start/Stop Selenium Server
#'
#' @description Provides a method to start and stop a selenium server. A lot of
#'   this function is hardcoded and may not work in all environments. It will be
#'   generalized have parameters added as it starts to get used more.
#'   
#' @details 
#' \describe{
#'   \item{seleniumStart()}{Starts a firefox server on the
#'   default port (4445). The function adds 2 objects to the global environment
#'   (\code{rD} and \code{remDr}) that are used to manipulate and interact with
#'   the server from your R code}
#'   \item{seleniumStop()}{Removes the created objects from the R environment and cleans 
#'   up the detached server with command to kill any open java.exe tasks. This 
#'   is the most dangerouse part of this funciton and only works on a windows 
#'   computer}
#' }
#' 
#' 
#' @rdname seleniumStartStop
#' @export
seleniumStart <- function() {
  #Open Browser Session and Selenium
  rD <<- RSelenium::rsDriver(port = 4445L, browser = "firefox", verbose = FALSE, iedrver = NULL, chromever = NULL, phantomver = NULL)
  remDr <- rD[["client"]]
  remDr$setTimeout(type = "page load", milliseconds = 20000)
  remDr <<- remDr
  return(NULL)
}



#' @rdname seleniumStartStop
#' @export
seleniumStop <- function() {
  #Close Browser Session and Selenium
  if ("remDr" %in% ls(name = .GlobalEnv)) {
    remDr$close()
    rm(list = c("remDr"), envir = .GlobalEnv)
  }
  if ("rD" %in% ls(name = .GlobalEnv)) {
    rD[["server"]]$stop()
    rm(list = c("rD"), envir = .GlobalEnv)
  }
  gc()
  x <- system("taskkill /im java.exe /f", intern=FALSE, ignore.stdout=FALSE)
  return(NULL)
}