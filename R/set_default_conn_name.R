#' Set the name of the default connection to use
#'
#' @param conn_name Character string. The name a connection
#'
#' @export
set_default_conn_name <- function(conn_name){

  if(!(conn_name %in% names(connection_cache))){

    stop(glue::glue("No connection named {conn_name}"))

  }

  defaults$default_conn_name <- conn_name
}

#' Get the name of the default connection
#' @return The name of the default connection
#' @rdname set_default_conn_name
#' @export
get_default_conn_name <- function(){

  defaults$default_conn_name
}
