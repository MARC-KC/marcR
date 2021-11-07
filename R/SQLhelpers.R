


#' @title SQL_newColumnFormatter
#' @description Creates the column definition part of a SQL query. Used as a
#'   helper for \code{SQL_createTable} and \code{SQL_addColumns}
#'
#' @param df data.frame/tibble. Optional. Must have df or types specified.
#' @param types Character vector of new column types (readr-like). Optional.
#'   Must have df or types specified.
#' @param charLength Vector of lengths used for character (NVARCHAR) types. If
#'   using the df argument, it will also accept a named vector of lengths for
#'   each column name with a character type.
#' @param colNames Character vector of columns names to include in database
#'   table. Ignored if using the df argument.
#'
#' @return A string containing partial SQL query for column definitions.
#'
#' @export
SQL_newColumnFormatter <- function(df = NULL, types = NULL, charLength, colNames = NULL) {
  
  
  if (!is.null(df)) {  
    
    #order columns to match names
    # df <- dplyr::select(df, all_of(names))
    
    #Check if colNames is empty 
    if (is.null(colNames)) {
      colNames <- names(df)
    }
    
    #change all factors to character
    df <- dplyr::mutate(df, dplyr::across(where(is.factor), as.character))
    
    
    #Check for Key-Values in charLength input and create charLengthVector
    if (!is.null(names(charLength))) {
      
      keyVal <- charLength
      charLength <- rep(NA_integer_, length(df))
      for (i in seq_along(keyVal)) {
        charLength[which(names(df) == names(keyVal)[i])] <- keyVal[i]
      }
    }
    
    #get column types
    type <- purrr::map_chr(df, ~class(.x)[1])
    
    #Check that all character columns have a charLength value
    if (any(is.na(charLength[type == 'character']))) {
      stop(paste0("There are character columns without a charLength value. ",
                  "Make sure you aren't forgetting factor variables as well."))
    }
    
    #ensure charLength is a character vector
    charLength <- dplyr::if_else(is.na(charLength), '', as.character(charLength))
    
    #create SQL types
    SQLtype <- dplyr::case_when(
      type == "character" ~ paste0("[NVARCHAR](", charLength, ")"),
      type == "integer" ~ "[INT]",
      type %in% c("numeric", "double") ~ "[REAL]",
      type == "logical" ~ "[BIT]",
      type == "Date" ~ "[DATE]",
      type == "POSIXct" ~ "[DATETIME]"
    )
    
    
  } else if (!is.null(types)) {
    
    #ensure charLength is a character vector
    charLength <- dplyr::if_else(is.na(charLength), '', as.character(charLength))
    
    #create SQL types
    SQLtype <- dplyr::case_when(
      types == "c" ~ paste0("[NVARCHAR](", charLength, ")"),
      types == "i" ~ "[INT]",
      types == "d" ~ "[REAL]",
      types == "l" ~ "[BIT]",
      types == "D" ~ "[DATE]",
      types == "T" ~ "[DATETIME]"
    )
  }
  
  
  innerSQL <- glue::glue('[{colNames}] {SQLtype}') %>% 
    glue::glue_collapse(sep = ' NULL,\n') %>% glue::glue(" NULL")
  
  return(innerSQL)
  
}




#' @title SQL_createTable
#' @description Creates raw SQL query to be used to create a table
#'
#' @param df data.frame/tibble. Optional. Must have df or types specified.
#' @param types Character vector of new column types (readr-like). Optional.
#'   Must have df or types specified.
#' @param charLength Vector of lengths used for character (NVARCHAR) types. If
#'   using the df argument, it will also accept a named vector of lengths for
#'   each column name with a character type.
#' @param colNames Character vector of columns names to include in database
#'   table. Ignored if using the df argument.
#' @param tableName Name of database table you want to create.
#' @param primaryKey Name of the primary key for the table. Default Null does
#'   not create one.
#'
#' @return A string containing the SQL code to create the specified table
#'
#' @section Creation notes: First created in 2020-12-15 while working in the
#'   MARC-KC/CovidDataEntry Repository
#'
#' @examples
#' \dontrun{
#' head(iris)
#' irisHelper <- tibble::tribble(
#'   ~name,              ~length,  ~type,
#'   'Sepal.Length',     NA,       'd',
#'   'Sepal.Width',      NA,       'd',
#'   'Petal.Length',     NA,       'd',
#'   'Petal.Width',      NA,       'd',
#'   'Species',          '10',     'c'
#' )
#' 
#' SQL_createTable(df = iris, charLength = c("Species" = 10),
#'                 tableName = "schema.iris", primaryKey = 'UniqueID')
#' SQL_createTable(types = irisHelper[['type']] , colNames = irisHelper[['name']],
#'                 charLength = irisHelper[['length']], tableName = "schema.iris")
#' }
#'
#' @export
SQL_createTable <- function(df = NULL, types = NULL, charLength, colNames = NULL, tableName, primaryKey = NULL) {

  #Format main columns
  innerSQL <- SQL_newColumnFormatter(df = df, types = types, colNames = colNames, charLength = charLength)

  #Add primary key if needed
  if (!is.null(primaryKey)) {
    innerSQL <-  glue::glue_collapse(c(
      glue::glue('[{primaryKey}] [INT] IDENTITY(1,1) PRIMARY KEY CLUSTERED'), 
      innerSQL), sep = ',\n')
  }
  
  #Format the full CREATE TABLE query
  SQLout <- glue::glue('CREATE TABLE {tableName} ( \n{marcR::glueIndentLines(innerSQL, 1)}\n);')
  
  return(SQLout)
}





#' @title SQL_addColumns
#' @description Creates raw SQL query to be used to add columns to a table
#'
#' @param df data.frame/tibble. Optional. Must have df or types specified.
#' @param types Character vector of new column types (readr-like). Optional.
#'   Must have df or types specified.
#' @param charLength Vector of lengths used for character (NVARCHAR) types. If
#'   using the df argument, it will also accept a named vector of lengths for
#'   each column name with a character type.
#' @param colNames Character vector of columns names to include in database
#'   table. Ignored if using the df argument.
#' @param tableName Name of database table you want to create.
#'
#' @return A string containing the SQL code to add the columns to the specified
#'   table
#'
#' @examples
#' \dontrun{
#' head(iris)
#' irisdf_addcolumns <- tibble::tibble(test = as.integer(1),
#'                                     test2 = 'test', test3 = TRUE, test4 = Sys.time())
#' irisHelper <- tibble::tribble(
#'   ~name,      ~length,  ~type,
#'   'test',     NA,       'i',
#'   'test2',    10,       'c',
#'   'test3',    NA,       'l',
#'   'test4',    NA,       'T',
#' )
#' SQL_addColumns(df = irisdf_addcolumns, charLength = c("test2" = 10), 
#'                tableName = "schema.iris")
#' SQL_addColumns(types = irisHelper[['type']], colNames = irisHelper[['name']],
#'                charLength = irisHelper[['length']], tableName = "schema.iris")
#' }
#'
#' @export
SQL_addColumns <- function(df = NULL, types = NULL, charLength, colNames = NULL, tableName) {
  
  #Format main columns
  innerSQL <- SQL_newColumnFormatter(df = df, types = types, colNames = colNames, charLength = charLength)
  
  #Format the full CREATE TABLE query
  SQLout <- glue::glue('ALTER TABLE {tableName}\nADD\n{marcR::glueIndentLines(innerSQL, 1)};')
  
  return(SQLout)
  
}

