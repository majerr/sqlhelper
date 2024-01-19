#' Close all connections and remove them from the connections cache
#'
#' @return `NULL`, invisibly
#' @examples
#' library(sqlhelper)
#' connect(
#'   system.file("examples",
#'               "sqlhelper_db_conf.yml",
#'               package="sqlhelper")
#' )
#' disconnect()
#' @export
disconnect <- function(){
  invisible(lapply(names(connection_cache),prune))
}

#' remove a connection from the connections cache
#'
#' @param conn_name The name of the connection to be removed
#'
#' @noRd
prune <- function(conn_name){

  if(!(conn_name %in% names(connection_cache)))
    stop(glue::glue("No connection named {conn_name}"))

  if(!connection_cache[[conn_name]]$pool && is_connected(conn_name))
    DBI::dbDisconnect(connection_cache[[conn_name]]$conn)


  connection_cache[[conn_name]] <- NULL
  rm(list=c(conn_name), envir=connection_cache)
}
