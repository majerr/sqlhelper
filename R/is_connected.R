#' Test whether a database is connected
#'
#' @param conn_name Character. The name of a connection (run [connection_info()]
#'   for options)
#'
#' @return Logical
#'
#' @details Note that these functions only report whether you are connected to a
#'   database with a connection named `conn_name`. A value of `TRUE` means that
#'   `conn_name` was found in the cache and is live, but  `FALSE` can mean that
#'   you not connected to _any_ databases, or that `conn_name` is not in the
#'   cache, or that it is in the cache but is not live. Use `connection_info()`
#'   if you need to determine which.
#'
#' @export
is_connected <- function(conn_name){
  live <- connection_info(conn_name)$live

  if(length(live) != 1)
    live = FALSE

  live
}

#' @rdname is_connected
#' @export
not_connected <- function(conn_name){
  !is_connected(conn_name)
}
