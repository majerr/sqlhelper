#' Set/get the name of the default connection to use
#'
#' @param conn_name Character string. The name a connection
#' @return \code{get} returns the name of the default connection; \code{set}
#'   returns \code{NULL}, invisibly.
#' @export
set_default_conn_name <- function(conn_name){

  if(!(conn_name %in% names(connection_cache))){

    stop(glue::glue("No connection named {conn_name}"))

  }

  defaults$default_conn_name <- conn_name
  invisible(NULL)
}

#' @rdname set_default_conn_name
#' @export
get_default_conn_name <- function(){

  defaults$default_conn_name

}
