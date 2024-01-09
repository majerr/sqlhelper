#' Test whether a database is connected
#'
#' @param conn_name Character. The name of a connection (run [connection_info()]
#'   for options)
#'
#' @return Logical, or NULL if `conn_name` does not identify exactly 1
#' connection
#'
#' @examples
#' library(sqlhelper)
#'
#' connect(
#'   system.file("examples/sqlhelper_db_conf.yml",
#'               package="sqlhelper")
#' )
#' connection_info()
#'
#' is_connected("simple_sqlite")
#' is_connected("foo")
#' DBI::dbDisconnect(live_connection("simple_sqlite"))
#' is_connected("simple_sqlite")
#' not_connected("simple_sqlite")
#' disconnect()
#' is_connected("simple_sqlite")
#' not_connected("simple_sqlite")
#' @export
is_connected <- function(conn_name){
  live <- connection_info(conn_name)$live

  if( !is.logical(live) || length(live) != 1)
    live = NULL

  live
}

#' @rdname is_connected
#' @export
not_connected <- function(conn_name){
  connected <- is_connected(conn_name)

  if( ! is.null(connected) ){
    return(!connected)
  }

  connected
}
