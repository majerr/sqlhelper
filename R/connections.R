
#' Test whether a database is connected
#'
#' @param conn_name Which database do you want to test? (restricted to 'cds' in MHCLG)
#'
#' @return Boolean
#'
#' is.connected and not.connected provide generic, semantically transparent
#' methods of checking whether a connection is live, irrespective of whether it
#' is through RODBC, RPostgreSQL, DBI etc. They depend on the is.live functions
#' provided by getrunparams (see above).

#' is.connected returns FALSE if the db connection returned by getrunparams is
#' not valid.
#' @export
is.connected <- function(conn_name){
  runparms <- getrunparams(conn_name)
  out <- NA
  tryCatch({out <- runparms$is.live(runparms$conn)},
           error = function(e){
             out<<-TRUE
           })

  return(out)
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

#' Return a list of available connection names
#'
#' @return A list of connections maintained by the loaded sqlhelper that are
#'   currently live.
#'
#' @export
connections_list <- function(){
  e <- get_cache_env()

  conn_names <- names(e$connections)

  if(length(conn_names)==0){
    return(character())
  }

  live_cons <- lapply(conn_names,is.connected)

  return(conn_names[live_cons != FALSE])
}

#' Return the named connection or NULL
#'
#' @param conn_name The name of the live connection you want (use
#'   connections_list to get names of available connections)
#'
#' @return A live connection to a database, or NULL
#'
#'   If \code{conn_name} is not the name of a live connection to a database, a
#'   warning is issued (by getrunparams via is.connected) and NULL is returned.
#'
#' @export
live_connection <- function(conn_name) {

  c <- NULL

  if(is.connected(conn_name)){

    c <- getrunparams(conn_name)$conn

  }

  return(c)

}
