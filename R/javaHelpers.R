#' @title Install Microsoft built OpenJDK Binaries
#'
#' @description Install a package level version of Java.
#'
#' @param installLocation The location you want to install Java to. Defaults to
#'   here::here("JavaEnv") in your project directory, but could also be stored
#'   to a 'renv' project directory, the user folder like
#'   '$USERPROFILE/AppData/Local'.
#' @param majorVersion A major java version to download. Must be one listed by
#'   `validJavaVersions()`.
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
#'         'APPDATA', 'Local', "MicrosoftOpenJDK"
#'     ),
#'     majorVersion = "17"
#' )
#' }
#' 
#' @describeIn java_install Install java to specified location.
#' @export
java_install <- function(installLocation = here::here("JavaEnv"), 
                         majorVersion = '11', 
                         os = c("windows", "linux", "macOS"),
                         arch = c("x64", "aarch64")) {
  
  os = match.arg(os)
  arch = match.arg(arch)
  validVersions <- suppressMessages(java_get_versions())
  if (!(as.character(majorVersion) %in% validVersions)) {
    stop(paste0('Valid versions are: [', paste0("'", validVersions, "'", collapse = ", "), "]"))
  }
  if (os != "windows"){
    stop("Function currently only set up to work on Windows OS.")
  }
  
  extension <- ifelse(os == "windows", ".zip", ".tar.gz")
  downloadURL <- paste0("https://aka.ms/download-jdk/microsoft-jdk-", majorVersion,"-", os, "-", arch, extension)
  downloadFile <- paste0(tempfile(pattern = paste0("Java_", majorVersion, "_")), extension)
  utils::download.file(downloadURL, destfile = downloadFile)
  utils::unzip(downloadFile, exdir = installLocation)
  message(paste0("Java extracted to: ", installLocation))
  
  #Add/update gitignore
  if (git2r::in_repository(installLocation)) {
    javaFolderName <- utils::unzip(downloadFile, list = TRUE)$Name[[1]]
    gitignoreFile <- file.path(installLocation, '.gitignore')
    if (!('.gitignore' %in% list.files(installLocation))) {
      write(javaFolderName, gitignoreFile)
    } else if (!(javaFolderName %in% readLines(gitignoreFile))) {
      write(javaFolderName, gitignoreFile, append = TRUE)
    }
    message("Java install added to .gitignore file.")
  }
  
  
}

#' @describeIn java_install Retrieve the versions of Java available at the download endpoint.
#' @export
java_get_versions <- function() {
  url = "https://docs.microsoft.com/en-us/java/openjdk/download"
  message(paste0("Checking ", url, " for downloadable versions."))
  rvest::read_html(url) %>% 
    rvest::html_elements("h3[id^=openjdk-]") %>% 
    rvest::html_attr("id") %>% 
    sub("^openjdk-", "", .) %>% 
    as.integer() %>% 
    sort() %>% 
    as.character()
}
