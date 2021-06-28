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
#' con <- connectODBC("<databaseName>.<schemaName>")
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
#' @param roundRealDigits Optionally round digits of all doubles. Takes an integer to fill in the `digits`
# argument of `round`. Default NULL causes this argument to be ignored.
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
#' con <- connectODBC("<databaseName>.<schemaName>")
#'
#' table <- DBI_getOBDCtable(con, "SELECT * FROM <schemaName>.<tableName>") 
#' }
#'
#' @export
DBI_getOBDCtable <- function(conn, query, roundRealDigits = NULL) {
  out <- query %>% 
    DBI::dbGetQuery(conn, .) %>% 
    tibble::as_tibble()
  
  if (!is.null(roundRealDigits)) {
    out <- out %>% roundReal(digits = roundRealDigits)
  }
  
  out
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
#' con <- connectODBC("<databaseName>.<schemaName>")
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
  dfTable[geomColStr] <- sf::st_as_text(dplyr::pull(dfTable, geomCol))
  dfTable <- sf::st_set_geometry(dfTable, NULL)
  
  #Create DBI name object for Temp table
  nameTemp <- paste0("##TempSpatial", format(Sys.time(), "%Y%m%d%H%M%S"), round(stats::runif(1,1,500)))
  
  
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
#' con <- connectODBC("<databaseName>.<schemaName>")
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
  
  out <- sf::st_read(conn, 
                     geometry_column = geomCol, 
                     query = query)
  
  return(out)
}


#' @title List all schema in database
#'
#' @description Searches schema in in the INFORMATION_SCHEMA.SCHEMATA table.
#'   Confirmed only to work with MS-SQL databases.
#'
#' @param conn A \code{\link[DBI:DBIConnection-class]{DBIConnection}} object, as
#'   returned by \code{\link[DBI:dbConnect]{dbConnect()}}.
#' @param rmSchemaRegex Character vector containing schema to avoid searching
#'   (removed schema regex). Ignores some default system level schema and schema
#'   only used by the ESRI SDE bindings that don't actually contain user created
#'   tables.
#'
#' @return A character vector of schema's in the database connections.
#'
#' @author Jacob Peterson
#'
#' @examples
#' \dontrun{
#'
#' con <- connectODBC("<databaseName>.<schemaName>")
#'
#' #This is not set up to be a real example
#' schemas <- dbListSchema(con)
#' }
#' @export
dbListSchema <- function(conn, rmSchemaRegex = c("sys", "sde", "^INFORMATION_SCHEMA$", "^db_\\.*")) {
  
  all_schemas <- DBI::dbGetQuery(conn, "SELECT SCHEMA_NAME FROM INFORMATION_SCHEMA.SCHEMATA")[['SCHEMA_NAME']]
  
  if (is.null(rmSchemaRegex)) {
    out <- all_schemas
  } else {
    out <- stringr::str_subset(all_schemas, pattern = paste0(rmSchemaRegex, collapse = "|"), negate = TRUE)
  }
  
  out    
}


#' @title List all tables in a database
#'
#' @description A more informative version of `DBI::dbListTables()` which only
#'   contains table names. This functions also pairs each table with its Schema
#'   and can handle checking if the table has spatial data or not.
#'
#' @param conn A \code{\link[DBI:DBIConnection-class]{DBIConnection}} object, as
#'   returned by \code{\link[DBI:dbConnect]{dbConnect()}}.
#' @param addGeoIndicator TRUE/FALSE. Should the `isSpatial` column be exported.
#'   Default is FALSE.
#' @param rmTableRegex Character vector containing table names to avoid
#'   searching (removed table regex). Ignores some tables that are only used by
#'   the ESRI SDE bindings that don't actually contain user created data.
#' @param ... Additional options passed to dbListSchema
#'
#' @return A dataframe with a row for each table in the database connection.
#'   Contains 2 or 3 columns ('schema', 'table', and optionally 'isSpatial').
#'   The return dataframe can then easily be searched, filtered, and queried to
#'   find the tables you were looking for.
#'
#' @author Jacob Peterson
#'
#' @examples
#' \dontrun{
#'
#' con <- connectODBC("<databaseName>.<schemaName>")
#'
#' #This is not set up to be a real example
#' tables <- dbListTableStructure(con, addGeoIndicator = TRUE)
#' }
#' @export
dbListTableStructure <- function(conn, addGeoIndicator = FALSE, rmTableRegex = c("^[:alpha:][:digit:]+$", "^SDE_", "^sysdiagrams$"), ...) {
  
  schemas <- dbListSchema(conn, ...)
  
  
  out <- purrr::map_dfr(schemas, ~{
    tables <- DBI::dbListTables(conn, schema = .x)
    if (length(tables) == 0) {
      out <- tibble::tibble(schema = character(), table = character())
    } else {
      out <- tibble::tibble(schema = .x, table = tables)
    }
    out
  })
  
  if (!is.null(rmTableRegex)) {
    out <- out %>% dplyr::filter(stringr::str_detect(table, paste0(rmTableRegex, collapse = "|"), negate = TRUE))
  }
  
  if (addGeoIndicator) {
    out[['isSpatial']] <-  purrr::pmap_lgl(out, function(schema, table, ...) {
      SQLquery <- glue::glue("
                       SELECT *  
                       FROM INFORMATION_SCHEMA.COLUMNS 
                       WHERE TABLE_SCHEMA = '{schema}' AND TABLE_NAME = '{table}' AND (DATA_TYPE = 'geometry' OR DATA_TYPE = 'geography')"
      )
      nrow(marcR::DBI_getOBDCtable(conn, SQLquery))>0
    })
  }
  
  out
  
}








