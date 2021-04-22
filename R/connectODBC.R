#' @title Connect to ODBC Database Using keyring
#'
#' @description Creates connection to ODBC database using the data contained in
#'   keyrings. This is generally used for internal purposes at MARC to make
#'   database connections a lazier proccess with {keryring}.
#'
#' @details Keys should be set up prior to using the function to a style similar
#'   to:\cr \code{keyring::key_set("DB_conn", username =
#'   "<databaseName>.<schemaName>")}\cr The password should then be set to the
#'   connection string: "Driver={ODBC Driver 17 for SQL
#'   Server};Server=<serverName>;Database=<databaseName>;UID=<schemaName>;PWD=<password>;"\cr
#'    Leave the PWD section in the username argument written as PASSWORD as this
#'   is automatically replaced with the stored password when calling
#'   \code{connectODBC()}
#'
#' @param databaseString Same as the username saved in the keyring. Default NULL
#'   will list the available keys under the service 'DB_conn'

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
#' con <- connectODBC("<databaseName>.<schemaName>")
#' 
#' DBI_getOBDCtable(con, "SELECT * FROM <schemaName>.<tableName>")
#' }

#' @md
#' @export
connectODBC <- function(databaseString = NULL) {
  
  #If input is null print saved connections
  if (is.null(databaseString)) {
    DB_Connections <- keyring::key_list() %>% 
      dplyr::filter(stringr::str_detect(service, "^DB_conn")) %>% 
      dplyr::pull(username) 
    stop(glue::glue("Keys available for the following database strings: '", 
                    glue::glue_collapse(DB_Connections, sep = "', '"), "'
                    Add more with `keyring::key_set()`"))
  }
  
  #If input is in old format stop and provide a message about the new format
  if (stringr::str_detect(databaseString, "DB_MARC")) {
    stop(paste0("Using keys in the format of 'DB_<DB_name>.<schema_name>' has been depreciated.\n", 
                "Use new format: service = 'DB_conn', username = '<DB_name>.<schema_name>'\n",
                "See example for new format."))
  }
  
  #Check that connection exists
  if(!(databaseString %in% keyring::key_list()[['username']])) {
    stop(glue::glue('
      Lazy server connection for {databaseString} not found.
      Consult documentation HERE to get this set up.'))
  }
  
  #Check that it only matches one record
  connectionKey <- keyring::key_list(service = "DB_conn") %>% 
    dplyr::filter(username == databaseString)
  if (nrow(connectionKey) != 1) {
    stop("databaseString matches more than one record in `keyring;:key_list`")
  }
  
  #Get connection string and create connection to database
  connectionString <- keyring::key_get(service = "DB_conn", username = databaseString)
  DBI::dbConnect(
    odbc::odbc(), 
    .connection_string = connectionString
  )
}