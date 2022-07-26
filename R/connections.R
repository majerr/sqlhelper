# This file contains several (mainly) internal functions used either by exported
# functions (e.g. those in defined in sqlrunners.R) or called by .onLoad()

#' Create the connection cache
#'
#' @return The new connection cache, invisibly
#' @import cachem
new_connection_cache <- function(){
  e <- parent.env(environment())
  e$connections <- cachem::cache_mem()
  invisible(e$connections)
}



conn_cache <- function(){
  encl <- parent.env(environment())
  encl$c2 <- cachem::cache_mem()
  encl$c2$set("con", 1)
}

conn_reset <- function(foo){
  encl <- parent.env(environment())
  encl$c2$set("con", foo)
}

conn_show <- function(){
  encl <- parent.env(environment())
  encl$c2$get("con")
}

#' Determine the connection driver
#'
#' @param conf A named list representing a single connection returned by
#'   \code{\link{get_all_configs}}.
#'
#' @details Search for an element named "server_type" and return a driver
#'   function appropriate for that database. For example, "server_type:
#'   sqlite" in your config will case \code{driver()} to return
#'   \code{\link[RSQLite]{SQLite}}.
#' @return A driver function. Defaults to \code{\link[odbc]{odbc}} if no
#'   "server_type" element is found.
#' @import dplyr stringr
driver <- function(conf){
  # Default is odbc
  if(!("server_type" %in% stringr::str_to_lower(names(conf)))){
    drv <- odbc::odbc
  } else {

    drv <- dplyr::case_when(
      stringr::str_to_lower(conf$server_type) == "sqlite" ~ list(RSQLite::SQLite),
      stringr::str_to_lower(conf$server_type) == "postgresql" ~ list(RPostgres::Postgres),
      stringr::str_to_lower(conf$server_type) == "mysql" |
        stringr::str_to_lower(conf$server_type) == "mariadb" ~ list(RMariaDB::MariaDB),
      stringr::str_to_lower(conf$server_type) == "bigquery" ~ list(bigrquery::bigquery),

      ### ...More patterns and drivers can be added here as needed... ###

      TRUE ~ list(odbc::odbc) # fallback is odbc if not recognized
    )[[1]] # The list wrappers and this de-listing subset are to avoid a 'not subsettable' error.
    # see https://github.com/tidyverse/dplyr/issues/5916
  }
  return(drv)
}

#' Populate the list of available connections
#'
#' @param config_filename String. Name of a config file. Defaults to NA. See
#'   \code{\link{read_config_file}} and \code{\link{get_all_configs}} for
#'   options.
#'
#' @param exclusive Boolean. Should this file be used exclusively to define
#'   connections? Defaults to \code{FALSE}. See \code{\link{get_all_configs}}.
#'
#' @details This function is called by \code{.onLoad()} and the exported
#'   \code{reconnect()} function (which closes existing connections before
#'   calling \code{set_connections()}).
set_connections <- function(config_filename=NA, exclusive=FALSE){

  conf <- validate_all_configs(
    get_all_configs(.fn=config_filename, exclusive = exclusive)
  )

  con_strs <- lapply(conf, yml2conn_str)

  default_conn_name <<- names(con_strs)[1]
  for(con_name in names(con_strs)){
    drv <- driver(conf[[con_name]])

    tryCatch({
      suppressWarnings({
        if(conf[[con_name]]$pool){
          connections[[con_name]] <<- pool::dbPool(drv(),
                                                   .connection_string=con_strs[[con_name]])
        } else {
          connections[[con_name]] <<- DBI::dbConnect(drv(),
                                                     .connection_string=con_strs[[con_name]])
        }
      })

    },
    error = function(c){
      message(c)
      connections[[con_name]] <- NA
      warning(glue::glue("{con_name} is not available"))
    })
  }

}



# Returns a connection and a sql runnner for the db parameter. For internal use
# only!
#
# For now, sqlhelpr only uses DBI so this isn't really necessary, but if in the
# future we need functions from other packages we can add them here without
# needing to touch anything else.
getrunparams <- function(conn_name){
  if(conn_name %in% names(connections)){
    return(
      list(
        conn = connections[[conn_name]],
        runner = DBI::dbGetQuery,
        is.live = DBI::dbIsValid)
    )
  }
  # Add more else if clauses here if more connections are required
  else{
    return(list(conn=NA,
                runner=NA))
  }
}

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
  runparms <- getrunparams(conn_name)
  out <- NA
  tryCatch({out <- runparms$is.live(runparms$conn)},
           error = function(e){
           print(runparms$is.live(runparms$conn))
             out<<-FALSE
           })
  return(!out)
}

#' Close all connections and remove them from the connections list
#'
#' This function is run when the library is unloaded
close_connections <- function(){
  for(conn_name in names(connections)){
    if(not.connected(conn_name)){
      next
    }
    if(methods::is(live_connection(conn_name),"Pool")){
      suppressWarnings(pool::poolClose(connections[[conn_name]]))
    } else {
      suppressWarnings(DBI::dbDisconnect(connections[[conn_name]]))
    }
  }

  if(environmentIsLocked(parent.frame())){
    unlockBinding("connections", parent.frame())
  }
  assign("connections",list(),envir=parent.frame())
  # connections <<- list()
}

#' Re-establish connections to all configured databases
#'
#' @param .fn String, the name of a config file
#' @param exclusive if .fn is present, should it be used exclusively (TRUE) or
#'   combined with user and site configs (FALSE)?
#' Closes and then re-opens all configured connections
#'
#' @export
reconnect <- function(.fn=NA, exclusive=FALSE){
  close_connections()
  set_connections(.fn, exclusive)
}

#' Return a list of available connection names
#'
#' @return A list of connections maintained by the loaded sqlhelper that are
#'   currently live.
#'
#' @export
connections_list <- function(){
  if(length(connections)==0){
    return(character())
  }
  live_cons <- lapply(names(connections),is.connected)
  return(names(connections)[live_cons != FALSE])
}

#' Return the named connection or NULL
#'
#' @param conn_name The name of the live connection you want (use
#'   connections_list to get names of available connections)
#'
#' @return A live connection to a database, or NULL
#'
#'   If \code{conn_name} is not the name of a live connection to a database, a
#'   warning is issued and NULL is returned.
#'
#' @export
live_connection <- function(conn_name) {
  live_cons <- lapply(names(connections),is.connected)
  if (conn_name %in% names(connections)[live_cons != FALSE]) {
    return(connections[[conn_name]])

  } else {
    warning(glue::glue("There is no live connection named '{conn_name}'"))
    return(NULL)
  }
}
