#' Test whether a database is connected
#'
#' @param conn_name Character. The name of a connection (run [connection_info()]
#'   for options)
#'
#' @return Logical, or NULL if `conn_name` does not identify exactly 1
#' connection
#'
#' @export
is_connected <- function(conn_name){
  live <- connection_info(conn_name)$live

  if( !is.null(live) && length(live) != 1)
    live = FALSE

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
