# This file contains several (mainly) internal functions used either by exported
# functions (e.g. those in defined in sqlrunners.R) or called by .onLoad()

set_cache_env <- function(){
  e <- parent.env(
    environment()
  )
  e$sqlhelper_conn_cache <- new.env(parent = emptyenv())
}

#' Access the environment containing the connections cache
#'
#' @return a sibling env to sqlhelper
get_cache_env <- function(){

  e <- parent.env(
        environment()
      )
  e$sqlhelper_conn_cache
}

#' Create the connection cache
#'
#' @return The new connection cache list, invisibly
new_connection_cache <- function(){
  e <- get_cache_env()

  e$connections <- list()

  invisible(e$connections)
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

#' Get/Set the default connection name
#'
#' @param new_default_name The name of a connection
set_default_conn_name <- function(new_default_name){

  e <- get_cache_env()

  # Can't set a default when there are no connections
  if(length(e$connections) == 0){
    warning(glue::glue("Could not set {new_default_name} as connection - no connections"))
    return(invisible(FALSE))
  }

  if(!(new_default_name %in% names(e$connections))){

    warning(
      glue::glue("Could not set connection '{new_default_name}' as \\
                       default: connection does not exist")
    )

    return(invisible(FALSE))

  }

  for(conn_name in names(e$connections)){
    e$connections[[conn_name]]$is_default <- FALSE
  }

  e$connections[[new_default_name]]$is_default <- TRUE
}

get_default_conn_name <- function(){
  e <- get_cache_env()

  default_conn_name <- NA

  for(conn_name in names(e$connections)){

    if(e$connections[[conn_name]]$is_default) {
      default_conn_name <- conn_name
      break
    }

  }

  return(default_conn_name)
}

#' Convert a (sub-list of a) list object returned by read_yaml() to a db
#' connection string.
#'
#' @param params Connection parameters
#'
#' @return Connection string
#'
#'   YAML values need to be double quoted in the YAML file.
#'
#'
yml2conn_str <- function(params){
  conparms <- params$connection
  paste0(
    paste0(
      names(conparms),
      "=",
      unlist(conparms)
    ),
    collapse="; ")
}

#' Add a new connection to the connections cache
#'
#' @param conn_name A name for the new connection
#' @param params Connection parameters
#'
#' @import pool DBI
add_connection <- function(conn_name, params){
  e <- get_cache_env()
  tryCatch({

    # The connection template is defined in data-raw/sysdata.R and loaded via
    # R/sysdata.rda
    new_conn <- connection_template

    new_conn$driver <- driver(params)

    if("pool" %in% names(params)){
      new_conn$pool <- params$pool
    } else {
      new_conn$pool <- FALSE
    }

    new_conn$conn_str <- yml2conn_str(params)

    if(new_conn$pool){
      new_conn$conn <- pool::dbPool(new_conn$driver(),
                                    .connection_string=new_conn$conn_str)
    } else {
      new_conn$conn <- DBI::dbConnect(new_conn$driver(),
                                      .connection_string=new_conn$conn_str)
    }

    e$connections[[conn_name]] <- new_conn
  },
  error=function(c){
    message(c)
    warning(glue::glue("{conn_name} is not available"))
  })

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

  # create connections cache. On failure, issue a warning, pass on the error
  # message and return FALSE invisibly, but do not stop.
  tryCatch({
    new_connection_cache()
  },
  error = function(c){
    warning("Could not create connections cache because:")
    message(c)
    return(invisible(FALSE))
  })

  for(conn_name in names(conf)){
    add_connection(conn_name, conf[[conn_name]])
  }

  default_conn_name <- names(conf)[1]
  set_default_conn_name(default_conn_name)
  invisible(TRUE)
}


#' Returns a connection and a sql runnner for the db parameter. For internal use
#' only!
#'
#' For now, sqlhelper only uses DBI so this isn't really necessary, but if in
#' the future we need functions from other packages we can add them here without
#' needing to touch anything else.
#'
#' @param conn_name the name of a connection
#'
#' @return A list, with elements 'conn' (the connection), 'runner' (the run
#'   function), and 'is.live' (a validity test for conn), or NULL if conn_name
#'   is not in the connections cache
getrunparams <- function(conn_name){

  e <- get_cache_env()

  p <- NULL
  if(conn_name %in% names(e$connections)){
    p <- list(
        conn = e$connections[[conn_name]]$conn,
        runner = DBI::dbGetQuery,
        is.live = DBI::dbIsValid)
  }

  # ... Add more else if clauses here if more connections are required

  else{
    warning(glue::glue("No connection named '{conn_name}'."))
  }

  return(p)
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
  return(!is.connected(conn_name))
}

#' remove a connection from the connections cache
#'
#' @param conn_name The name of the connection to be removed
prune <- function(conn_name){

  e <- get_cache_env()

  if(is.connected(conn_name)){
    if(e$connections[[conn_name]]$pool){

      suppressWarnings(pool::poolClose(e$connections[[conn_name]]$conn))

    } else {

      suppressWarnings(DBI::dbDisconnect(e$connections[[conn_name]]$conn))

    }
  }

  e$connections[conn_name] <- NULL

  # reset the default in case it was the default that got pruned
  default_conn_name <- names(e$connections)[1]
  if(!is.na(default_conn_name)){
    set_default_conn_name(default_conn_name)
  }

}

#' Close all connections and remove them from the connections list
#'
#' This function is run when the library is unloaded
close_connections <- function(){

  e <- get_cache_env()

  for(conn_name in names(e$connections)){
    prune(conn_name)
  }

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
