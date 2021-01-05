#' @title SQL_createTable
#' @description Creates raw SQL code to be used to create a table
#' 
#' @param df data.frame/tibble. Optional. Must have df or types specified
#' @param types Character vector of new column types (readr-like). Optional. Must have df or types specified
#' @param names Character vector of columns names to include in database table
#' @param charLength Vector of lengths used for character (NVARCHAR) types.
#' @param tableName Name of database table you want to create.
#' @param primaryKey Name of the primary key for the table. Default Null does not create one.
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
#' SQL_createTable(df = iris, names = irisHelper[['name']], charLength = irisHelper[['length']], tableName = "schema.iris", primaryKey = 'UniqueID')
#' SQL_createTable(types = irisHelper[['type']] , names = irisHelper[['name']], charLength = irisHelper[['length']], tableName = "schema.iris")
#' }
#' 
#' @export
SQL_createTable <- function(df = NULL, types = NULL, names, charLength, tableName, primaryKey = NULL) {


#ensure charLength is a character vector
charLength <- dplyr::if_else(is.na(charLength), '', as.character(charLength))



if (!is.null(df)) {  
  
  #order columns to match names
  df <- dplyr::select(df, all_of(names))
  
  #change all factors to character
  df <- dplyr::mutate(df, dplyr::across(where(is.factor), as.character))
  
  #get column types
  type <- purrr::map_chr(df, ~class(.x)[1])
  
  
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



innerSQL <<- glue::glue('[{names}] {SQLtype}') %>% 
  glue::glue_collapse(sep = ' NULL,\n') %>% glue::glue(" NULL")

# [UniqueID] [int] IDENTITY(1,1) PRIMARY KEY CLUSTERED,

if (!is.null(primaryKey)) {
  innerSQL <-  glue::glue_collapse(c(glue::glue('[{primaryKey}] [INT] IDENTITY(1,1) PRIMARY KEY CLUSTERED'), innerSQL), sep = ',\n')
}

SQLout <- glue::glue('CREATE TABLE {tableName} ( \n{marcR::glueIndentLines(innerSQL, 1)}\n)')

return(SQLout)
}
