
#' Test whether a database is connected
#'
#' @param conn_name Which connection do you want to test? (run
#'   \link{\code{connection_info}} for options)
#'
#' @return Logical
#'
#'   is.connected and not.connected are syntactic sugar around connection_info().
#'
#' @export
is.connected <- function(conn_name){
  conns <- connection_info(conn_name)

  if(nrow(conns) == 0){
    stop(glue::glue("No connection named {conn_name}"))

  } else if(nrow(conns) > 1){
    stop(
      glue::glue("\"{conn_name}\" matched multiple connection names:\\
                 {\"\t\"}{paste0(conns$name, collapse=', ')}")
      )
  }

  conns$live
}

#' Test whether a database is connected
#'
#' @param db Which database do you want to test?
#'
#' @return Boolean
#'
#' is.connected and not.connected provide generic, semantically transparent
#' methods of checking whether a connection is live, irrespective of whether it
#' is through RODBC, RPostgreSQL, DBI etc. They depend on the is.live functions
#' provided by getrunparams (see above).
#'
#' not.connected returns TRUE if the db connection returned by getrunparams is
#' not valid
#' @export
not.connected <- function(conn_name){
  return(!is.connected(conn_name))
}



#' Re-establish connections to all configured databases
#'
#' @param .fn String, the name of a config file
#' @param exclusive if .fn is present, should it be used exclusively (TRUE) or
#'   combined with user and site configs (FALSE)?
#' Closes and then re-opens all configured connections
#'
#' @export
reconnect <- function(.config_filename=NA, exclusive=FALSE){
  close_connections()
  create_connections(.config_filename, exclusive)
}



#' Return the named connection or NULL
#'
#' @param conn_name Chr. The name of the live connection you want (use
#'   \link{\code{connection_info}} to get names of available connections).
#'
#' @return A live connection to a database, or NULL
#'
#'
#'   If \code{conn_name} is not the name of a live connection to a database, a
#'   warning is issued (by getrunparams via is.connected) and NULL is returned.
#'
#' @export
live_connection <- function(conn_name) {
  tryCatch({
    if(is.connected(conn_name)){
      return(connection_cache[[conn_name]]$conn)
    }
  },
  error = function(e){
    return(NULL)
  })
}
