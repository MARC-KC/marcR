#' @title Easily Download Open Street Map Data
#'
#' @description Downloads multiple vector datasets from Open Street Maps given a
#'   dataframe of key-value pairs and a bounding box.
#'
#' @param keyValDF Dataframe with two columns, 'key' and 'value'. Can be created
#'   with [osm_keyValueDF()]
#' @param  bbox the bounding box for the download area
#'
#' @return A list with class osmdata containing the spatial data within its
#'   elements.
#'
#' @section Creation notes: First created in 2020-11-03 with the script
#'   OSMdownloadAndSFtoSQLInterface.R in the MARC-KC/HelpLibrary Repository
#'
#' @author Jacob Peterson
#'
#' @examples
#' \dontrun{
#' library(sf)
#' # devtools::install_github('yonghah/esri2sf')
#' library(esri2sf)
#' library(dplyr)
#' library(osmdata)
#'
#'
#' #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#' # Download MARC Boundaries ####
#' #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#' MARCprojection <- paste0(
#' "+proj=tmerc +lat_0=36.16666666666666 +lon_0=-94.5 +k=0.9999411764705882 ", 
#' "+x_0=850000 +y_0=0 +ellps=GRS80 +datum=NAD83 +to_meter=0.3048006096012192 ", 
#' "+no_defs")
#'
#'
#' #For fixing column names form the table joins
#' removeColnamePrefix <- function(df) {
#'   names(df) <- sub('.*\\.', '', names(df)) %>%
#'     ave(., ., FUN = function(i) paste0(i, '_', seq_along(i))) %>%
#'     gsub("_1$", "", .)
#'   df
#' }
#'
#' MARCjurisdictions <- esri2sf::esri2sf(url = 
#' "https://gis2.marc2.org/arcgis/rest/services/HumanServices/COVIDv2/MapServer/0") %>%
#'   sf::st_make_valid() %>% #fixes some issues with data coming from ESRI
#'   sf::st_transform(MARCprojection) %>%#puts it in MARC cordinates
#'   removeColnamePrefix() #For fixing column names form the table joins
#' #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#'
#' #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#' # Create Bounding Box ####
#' #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#' bb_sf <- MARCjurisdictions %>% sf::st_transform(crs = 4326) %>% sf::st_bbox()
#' #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#'
#'
#' #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#' # Download Data for multiple Key Value Sets ####
#' #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#' df <- tibble::tribble(
#'   ~key,            ~value,
#'   "highway",       "motorway",
#'   "highway",       "trunk",
#'   "highway",       "primary",
#' )
#'
#'
#' #download data
#' test <- osm_keyValueDL(df, bb_sf)
#'
#' #simple plot
#' test$osm_lines %>% sf::st_geometry() %>% plot()
#' #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#' }
#' @export
osm_keyValueDL <- function(keyValDF, bbox) {
  
  if (!requireNamespace("osmdata", quietly = TRUE)) {
    stop("Package \"osmdata\" needed for this function to work. Please install it.",
         call. = FALSE)
  }
  
  
  keyValDF <- keyValDF %>% dplyr::mutate(id = 1:n())
  
  outDF <- keyValDF %>% dplyr::mutate(osm = purrr::map2(key, value, ~ {
    cat(crayon::blue("Downloading", 
                     keyValDF[keyValDF$key == .x & keyValDF$value == .y, "id"], 
                     "/", nrow(keyValDF), "key =", .x, "; value =", .y, "\n"))
    osmdata::opq(bbox) %>% 
      osmdata::add_osm_feature(key = .x, value = .y) %>% 
      osmdata::osmdata_sf()
  }))
  
  out <- do.call(c, outDF$osm)
  return(out)
}






#' @title Retrieve all Key-Value Pairs Open Street Map Data
#'
#' @description Downloads and creates a dataframe for all key-value pairs in OSM
#'   data. This function is parameterless.
#'
#' @return A data frame with two columns, 'key' and 'value'.
#'
#' @section Creation notes: First created in 2020-11-03 with the script
#'   OSMdownloadAndSFtoSQLInterface.R in the MARC-KC/HelpLibrary Repository
#'
#' @author Jacob Peterson
#'
#' @examples
#' \dontrun{
#' library(purrr)
#' library(tibble)
#' library(osmdata)
#' 
#' df <- osm_keyValueDF()
#' df
#' }
#' @export
osm_keyValueDF <- function() {
  
  if (!requireNamespace("osmdata", quietly = TRUE)) {
    stop("Package \"osmdata\" needed for this function to work. Please install it.",
         call. = FALSE)
  }
  
  keyValueDF <- purrr::map_dfr(osmdata::available_features(), ~ {
    df <- tibble::tibble(key = .x, value = osmdata::available_tags(.x))
    if (nrow(df) == 0 ) {
      df <- tibble::tibble(key = .x, value = NA_character_)
    }
    return(df)
  })
  
  return(keyValueDF)
}






