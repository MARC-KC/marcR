#' @title Connect to ODBC Database Using keyring
#'
#' @description Creates connection to ODBC database using the data contained in
#' keyrings. This is generally used for internal purposes at MARC to make
#' database connections a lazier proccess with {keryring}.
#'
#' @details Keys should be set up prior to using the function to a style similar
#' to:\cr \code{keyring::key_set("DB_<databaseName>.<schemaName>", username =
#' "Driver={ODBC Driver 17 for SQL
#' Server};Server=<serverName>;Database=<databaseName>;UID=<schemaName>;PWD=<password>;")}\cr
#' Leave the PWD section in the username argument written as PASSWORD as this is
#' automatically replaced with the stored password when calling
#' \code{connectODBC()}
#'
#' @param databaseString Same as the service saved in the keyring. Default NULL
#'   will list the available keys prefixed with 'DB_'

#'
#' @return OBDC connection object returned from DBI::dbConnect()
#'
#' @section Creation notes: First created in 2020-Sept for easily accessing
#'   databases in R
#'
#' @author Jacob Peterson
#'
#' @examples
#' \dontrun{
#' library(glue)
#' library(magrittr)
#' library(DBI)
#'
#' con <- connectODBC("DB_<databaseName>.<schemaName>")
#'
#' glue::glue("
#' SELECT *
#'   FROM <schemaName>.<tableName>
#' ") %>%
#'   DBI::dbGetQuery(con, .)
#' }

#' @export
connectODBC <- function(databaseString = NULL) {
  if (is.null(databaseString)) {
    DB_Connections <- keyring::key_list() %>% dplyr::pull(service) %>% 
      stringr::str_subset("^DB_")
    stop(glue::glue("Keys available for the following database strings: '", 
                    glue::glue_collapse(DB_Connections, sep = "', '"), "'
                    Add more with `keyring::key_set()`"))
  }
  
  if(!(databaseString %in% keyring::key_list()[['service']])) {
    stop(glue::glue('
Lazy server connection for {databaseString} not found.
Consult Jacob Peterson to get this set up.'))
  }
  
  
  connectionString <- keyring::key_list(service = databaseString) %>% 
    dplyr::filter(stringr::str_detect(username, "^$", negate = TRUE))
  if (nrow(connectionString) != 1) {
    stop("databaseString matches more than one record in `keyring;:key_list`")
  }
  connectionString <- connectionString$username[1] %>% 
    stringr::str_replace("PASSWORD;", 
                         keyring::key_get(service = databaseString, 
                                          username = connectionString$username[1]))
  
  DBI::dbConnect(odbc::odbc(), 
                 .connection_string = connectionString)
}