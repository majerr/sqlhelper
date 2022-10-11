#' Test whether a database is connected
#'
#' @param conn_name Character. The name of a connection (run
#'   [connection_info()] for options)
#'
#' @return Logical, or NULL if \code{conn_name} does not identify exactly 1
#' connection
#'
#' @details `not_connected` is provided as a convenience
#'
#' @export
is_connected <- function(conn_name){

  live <- connection_info(conn_name)$live

  if(length(live) != 1){
    return(NULL)
  }

  live
}

#' @rdname is_connected
#' @export
not_connected <- function(conn_name){
  return(!is_connected(conn_name))
}
