#' Get information about available database connection
#'
#' @param name_str A regular expression to be used identify connection names to
#'   include. The default ('.*') returns all of them.
#'
#' @return A tibble with the following columns:
#'
#'     * name - identifier (character)
#'     * description - a description of the connection, if found in the conf file (character)
#'     * live - is this connection valid and live? (logical)
#'     * driver - the name of the driver used (character)
#'     * conn_str - the string used to parameterize the connection (character)
#'     * pool - is this a pool connection? (boolean)
#'
#'
#' @export
connection_info <- function(name_str = ".*"){

  conn_names <- stringr::str_subset(names(connection_cache),name_str)

  conn_table <- tibble::tibble(
    name = character(),
    description = character(),
    live = logical(),
    driver = character(),
    conn_str = character(),
    pool = logical()
  )

  for(name in conn_names){

    conn_data <- connection_cache[[name]]
    conn_data$live <- DBI::dbIsValid(conn_data$conn)
    conn_data$conn <- NULL # as_tibble_row will complain otherwise
    conn_data$name <- name

    conn_data <- dplyr::relocate(
      tibble::as_tibble_row(conn_data),
        names(conn_table)
    )[1:ncol(conn_table)] #Drop any cols not required

    conn_table <- tibble::add_row(conn_table,conn_data)
  }

  return(conn_table)
}
