#' @title Retrieve Column Names from an Microsoft SQL Connection
#'
#'
#' @param conn A \code{\link[DBI:DBIConnection-class]{DBIConnection}} object, as
#'   returned by \code{\link[DBI:dbConnect]{dbConnect()}}.
#' @param schema Name of schema that table is under. character vector of length
#'   1.
#' @param tableName Name of table. character vector of length 1.
#'
#' @return Character vector of column names in specified table.
#'
#' @section Creation notes: First created on 2020-11-03 with the script
#'   OSMdownloadAndSFtoSQLInterface.R in the MARC-KC/HelpLibrary Repository
#'
#' @author Jacob Peterson
#'
#' @examples
#' \dontrun{
#' library(DBI)
#'
#' con <- connectODBC("DB_<databaseName>.<schemaName>")
#'
#' tableColNames <- DBI_getColNames(con, "<schemaName>", "<tableName>")
#' }
#' @export
DBI_getColNames <- function(conn, schema, tableName) {
  out <- DBI::dbGetQuery(conn, glue::glue("SELECT TOP 0 * FROM {schema}.{tableName}")) %>% names()
  return(out)
}


#' @title Get Table from OBDC connection
#'
#' @param conn A \code{\link[DBI:DBIConnection-class]{DBIConnection}} object, as
#'   returned by \code{\link[DBI:dbConnect]{dbConnect()}}.
#' @param query SQL Query to server to request table
#'
#'
#' @return Tibble of a database table
#'
#' @section Creation notes: First created in 2020-12-11 while working in the
#'   MARC-KC/CovidDataEntry Repository
#'
#' @author Jacob Peterson
#'
#' @examples \dontrun{ 
#' 
#' library(DBI)
#'
#' con <- connectODBC("DB_<databaseName>.<schemaName>")
#'
#' table <- DBI_getOBDCtable(con, "SELECT * FROM <schemaName>.<tableName>") 
#' }
#'
#' @export
DBI_getOBDCtable <- function(conn, query) {
  data <- query %>% 
    DBI::dbGetQuery(conn, .) %>% 
    tibble::as_tibble()
}




#' @title Append sf table to a Microsoft SQL table
#'
#'
#' @param conn A \code{\link[DBI:DBIConnection-class]{DBIConnection}} object, as returned by \code{\link[DBI:dbConnect]{dbConnect()}}.
#' @param sfTable sf object to append to specified database table
#' @param schema Name of schema that table is under. character vector of length
#'   1.
#' @param tableName Name of table. character vector of length 1.
#' @param createTableQuery Currently unused. Always NULL
#' @param warnings Warning key. Use `-1` to hide warning messages in function.
#'
#' @return NULL
#'
#' @section Creation notes: First created on 2020-11-03 with the script
#'   OSMdownloadAndSFtoSQLInterface.R in the MARC-KC/HelpLibrary Repository
#'
#' @author Jacob Peterson
#'
#' @examples
#' \dontrun{
#' library(DBI)
#'
#' con <- connectODBC("DB_<databaseName>.<schemaName>")
#' 
#' #This is not set up to be a real example
#' tableColNames <- DBI_appendSFtoTable(con, sfTable, "<schemaName>", "<tableName>")
#' }
#' @export
DBI_appendSFtoTable <- function(conn, sfTable, schema, tableName, createTableQuery = NULL, warnings = getOption("warn")) {
  
  #Suppress Warnings
  warnDef = getOption("warn")
  options(warn=warnings)
  
  #Create DBI name object for Main table and Check it exists
  name <- DBI::Id(schema = schema, table = tableName)
  
  ##add option to create table if doesn't exist
  if (!DBI::dbExistsTable(conn = conn, name = name)) stop(glue::glue("The table at {schema}.{tableName} doesn't exist. Create this table before running this function."))
  
  #What is the geometry column
  geomCol <- sfTable %>% purrr::map_lgl(~class(.x) %>% stringr::str_detect("sfc") %>% any()) %>% which() %>% names()
  if (length(geomCol) != 1) stop("Multiple sfc Columns Detected")  
  
  #Create Temp DF 
  geomColStr <- paste0(geomCol, "_str")
  dfTable <- sfTable 
  dfTable[geomColStr] <- sf::st_as_text(pull(dfTable, geomCol))
  dfTable <- sf::st_set_geometry(dfTable, NULL)
  
  #Create DBI name object for Temp table
  nameTemp <- paste0("##TempSpatial", format(Sys.time(), "%Y%m%d%H%M%S"), round(runif(1,1,500)))
  
  
  #Create Temp SQL Table
  DBI::dbCreateTable(conn = conn,
                     name = nameTemp,
                     fields = dfTable)
  
  
  #Write Data to Temp SQL Table
  DBI::dbAppendTable(conn = conn,
                     name = nameTemp,
                     value = dfTable)
  
  
  #Create Append Query With Conversion
  tableColNames <- DBI_GetColNames(conn, schema, tableName)
  tableColStrMain <- tableColNames %>% glue::glue_collapse(sep = "], [")
  tableColStrTemp <- tableColNames %>% 
    stringr::str_replace(geomCol, glue::glue("geometry::STGeomFromText([{geomColStr}], 0) AS [{geomCol}")) %>% 
    glue::glue_collapse(sep = "], [") %>% 
    stringr::str_replace("\\[geometry::STGeomFromText", "geometry::STGeomFromText")
  
  AppendQuery <- glue::glue("
  INSERT INTO [{schema}].[{tableName}]([{tableColStrMain}])
  SELECT [{tableColStrTemp}]
  FROM [{nameTemp}]
  ")
  
  
  #Append data from temp table to main table
  DBI::dbGetQuery(conn = conn, statement = AppendQuery) 
  
  
  #Delete Temp Table
  DBI::dbRemoveTable(conn = conn, name = nameTemp)
  
  #End Suppress warnings
  options(warn=warn)
  
}





#' @title Read Spatial SQL Table to sf
#'
#' @description Is basically a wrapper around
#'   \code{\link[sf:st_read]{sf::st_read()}} that does a necessary
#'   transformation on the SQL geometry datatype to read in as a sf table
#'
#' @param conn A \code{\link[DBI:DBIConnection-class]{DBIConnection}} object, as
#'   returned by \code{\link[DBI:dbConnect]{dbConnect()}}.
#' @param schema Name of schema that table is under. character vector of length
#'   1.
#' @param tableName Name of table. character vector of length 1.
#' @param geomCol Character vector of length 1 containing the name of the
#'   geometry column in the SQL table
#'
#' @return An sf object
#'
#' @section Creation notes: First created in 2020-11-03 with the script
#'   OSMdownloadAndSFtoSQLInterface.R in the MARC-KC/HelpLibrary Repository
#'
#' @author Jacob Peterson
#'
#' @examples
#' \dontrun{
#' library(DBI)
#'
#' con <- connectODBC("DB_<databaseName>.<schemaName>")
#'
#' #This is not set up to be a real example
#' tableColNames <- sf_readSQL(con, "<schemaName>", "<tableName>", "geom")
#' }
#' @export
sf_readSQL <- function(conn, schema, tableName, geomCol) {
  
  tableColNames <- DBI_getColNames(conn, schema, tableName)
  
  tableColStr <- tableColNames %>% stringr::str_replace(geomCol, glue::glue("{geomCol}].STAsBinary() AS [{geomCol}")) %>% glue::glue_collapse(sep = "], [")
  
  query <- glue::glue("
SELECT [{tableColStr}]
FROM {schema}.{tableName}
")
  
  out <- st_read(conn, 
                 geometry_column = geomCol, 
                 query = query)
  
  return(out)
}


