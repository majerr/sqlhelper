#' Set/get the name of the default connection to use
#'
#' @param conn_name Character string. The name a connection
#'
#' @return \code{get} returns the name of the default connection;
#'      \code{set} returns \code{NULL}, invisibly.
#'
#' @examples
#' library(sqlhelper)
#' connect(
#'     system.file("examples/sqlhelper_db_conf.yml",
#'                 package="sqlhelper"),
#'     exclusive = TRUE
#' )
#'
#' connection_info()
#'
#' get_default_conn_name()
#'
#' set_default_conn_name("pool_sqlite")
#'
#' connection_info()
#'
#' get_default_conn_name()
#'
#' @export
set_default_conn_name <- function(conn_name){

  if(!(conn_name %in% names(connection_cache))){

    stop(glue::glue("No connection named {conn_name}"))

  }

  defaults$default_conn_name <- conn_name
  invisible(NULL)
}

#' @rdname set_default_conn_name
#'
#' @export
get_default_conn_name <- function(){

  defaults$default_conn_name

}
