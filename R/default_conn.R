#' Return the default connection
#'
#' A convenience wrapper around `live_connection()` and `get_default_conn_name()`
#'
#' @return A database connection returned by `DBI::dbConnect()` or
#'   `pool::dbPool()`
#' @export
default_conn <- function(){
  live_connection( get_default_conn_name() )
}
