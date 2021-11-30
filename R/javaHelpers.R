#' @title Install Microsoft or Adoptium built OpenJDK Binaries
#'
#' @description 
#' `r lifecycle::badge('maturing')`  
#' 
#' Install a package level version of Java.
#'
#' @param installLocation The location you want to install Java to. Defaults to
#'   here::here("JavaEnv") in your project directory, but could also be stored
#'   to a 'renv' project directory, the user folder like
#'   '$USERPROFILE/AppData/Local'.
#' @param majorVersion A major java version to download. Must be one listed by
#'   `validJavaVersions()`.
#' @param source What binary version do you want to install? Either 'microsoft'
#'   or 'adoptium'.
#' @param os One of 'windows', 'linux', or 'macOS'. Currently only 'windows' is
#'   valid.
#' @param arch One of 'x64' or 'aarch64'. 'x64' is default.
#'
#' @return Adds the unzipped/untared package to the location specified by
#'   installLocation.
#' @examples
#' \dontrun{
#' java_install(
#'     installLocation = file.path(
#'         normalizePath(Sys.getenv("USERPROFILE"), winslash = "/"),
#'         'APPDATA', 'Local', "r-OpenJDK"
#'     ),
#'     majorVersion = "17", 
#'     source = "adoptium"
#' )
#' }
#'
#' @describeIn java_install Install java to specified location.
#' @export
java_install <- function(installLocation = here::here("JavaEnv"), 
                         majorVersion = '11', 
                         source = c("microsoft", "adoptium"),
                         os = c("windows", "linux", "macOS"),
                         arch = c("x64", "aarch64")) {
  
  os = match.arg(os)
  arch = match.arg(arch)
  source = match.arg(source)
  validVersions <- suppressMessages(java_get_versions(source = source))
  if (!(as.character(majorVersion) %in% validVersions)) {
    stop(paste0('Valid versions are: [', paste0("'", validVersions, "'", collapse = ", "), "]"))
  }
  if (os != "windows"){
    stop("Function currently only set up to work on Windows OS.")
  }
  
  extension <- ifelse(os == "windows", ".zip", ".tar.gz")
  if (source == 'microsoft') {
    downloadURL <- paste0("https://aka.ms/download-jdk/microsoft-jdk-", majorVersion,"-", os, "-", arch, extension)
    # checksum <- ''
  } else if (source == 'adoptium') {
    queryUrl <- 'https://api.adoptium.net'
    os <- if (os == 'macOS') 'mac' else os
    requestURL <- paste0(queryUrl, "/v3/assets/feature_releases/", majorVersion, "/ga?architecture=", arch, "&image_type=jdk&os=", os, "&project=jdk&sort_method=DATE&sort_order=DESC&vendor=eclipse")
    requestResponse <- jsonlite::fromJSON(httr::content(httr::GET(requestURL), type = 'text', encoding = 'UTF-8'))
    downloadURL <- requestResponse[['binaries']][[1]][['package']][['link']]
    checksum <- requestResponse[['binaries']][[1]][['package']][['checksum']]
  }
  downloadFile <- paste0(tempfile(pattern = paste0("Java_", majorVersion, "_")), extension)
  utils::download.file(downloadURL, destfile = downloadFile)
  utils::unzip(downloadFile, exdir = installLocation)
  message(paste0("Java extracted to: ", installLocation))
  
  #Add/update gitignore
  if (git2r::in_repository(installLocation)) {
    javaFolderName <- utils::unzip(downloadFile, list = TRUE)$Name[[1]]
    gitignoreFile <- file.path(installLocation, '.gitignore')
    if (!('.gitignore' %in% list.files(installLocation, all.files = TRUE))) {
      write(javaFolderName, gitignoreFile)
    } else if (!(javaFolderName %in% readLines(gitignoreFile))) {
      write(javaFolderName, gitignoreFile, append = TRUE)
    }
    message("Java install added to .gitignore file.")
  }
  
  
}

#' @describeIn java_install Retrieve the versions of Java available at the download endpoint.
#' @export
java_get_versions <- function(source = c("microsoft", "adoptium")) {
  source = match.arg(source)
  
  if (source == 'microsoft') {
    url <- "https://docs.microsoft.com/en-us/java/openjdk/download"
    out <- rvest::read_html(url) %>% 
      rvest::html_elements("h3[id^=openjdk-]") %>% 
      rvest::html_attr("id") %>% 
      sub("^openjdk-", "", .) %>% 
      as.integer() %>% 
      sort() %>% 
      as.character()
  } else if (source == 'adoptium') {
    url <- "https://adoptium.net/releases.html"
    queryUrl <- 'https://api.adoptium.net'
    versions <- jsonlite::fromJSON(httr::content(httr::GET(paste0(queryUrl, "/v3/info/available_releases")), type = 'text', encoding = 'UTF-8'))
    out <- versions[['available_releases']] %>% sort() %>% as.character()
  }
  message(paste0("Checking ", url, " for downloadable versions."))
  out
}
