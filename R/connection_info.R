#' Browse available connections
#'
#' Provides information about created connections.
#'
#' @param name_str A regular expression to be used to identify connection names
#'   to include. The default ('.*') returns all of them.
#' @param exact TRUE or FALSE. Should `name_str` match the name of a connection
#'   exactly? TRUE will identify only 1 connection if name_str does not contain
#'   any metacharacters
#'
#' @return Null, or a tibble with 1 row per identified connection and the
#'   following fields:
#'
#'\describe{
#'  \item{name}{identifier (character)}
#'  \item{description}{a description of the connection, if found in the conf file (character)}
#'  \item{live}{is this connection valid and live? (logical)}
#'  \item{driver}{the name of the driver function (character)}
#'  \item{conn_str}{the string used to parameterize the connection (character)}
#'  \item{pool}{is this a pool connection? (logical)}
#' }
#'
#' If no connection names matched `name_str`, the tibble will be empty. If
#' no connections have been configured (e.g. `connect()` has not been called), `NULL` is returned.
#'
#' @export
connection_info <- function(name_str = ".*", exact = TRUE){

  if(length(names(connection_cache)) == 0){
    return(NULL)
  }

  if(exact)
    name_str <- paste0("^",name_str, "$")
  conn_names <- stringr::str_subset(names(connection_cache),name_str)

  conn_table <- tibble::tibble(
    name = character(),
    description = character(),
    live = logical(),
    default = logical(),
    driver = character(),
    conn_str = character(),
    pool = logical()
  )

  for(name in conn_names){

    conn_data <- connection_cache[[name]]
    conn_data$live <- DBI::dbIsValid(conn_data$conn)
    conn_data$conn <- NULL # as_tibble_row will complain otherwise
    conn_data$name <- name
    conn_data$default <- ( name == get_default_conn_name() )

    conn_data <- dplyr::relocate(
      tibble::as_tibble_row(conn_data),
        names(conn_table)
    )[1:ncol(conn_table)] #Drop any cols not required

    conn_table <- tibble::add_row(conn_table,conn_data)
  }


  conn_table

}
