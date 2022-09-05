
#########################################################
####### Internal functions related to connections #######
#########################################################

#' Determine the connection driver
#'
#' @param conf A named list representing a single connection returned by
#'   \code{\link{validate_configs}}.
#'
#' @details Search for an element named "server_type" and return a driver
#'   function appropriate for that database. For example, "server_type:
#'   sqlite" in your config will cause \code{driver()} to return
#'   \code{\link[RSQLite]{SQLite}}.
#'
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
    )[[1]]
    # The list wrappers inside case_when and this de-listing subset are to avoid
    # a 'not subsettable' error. see
    # https://github.com/tidyverse/dplyr/issues/5916
  }
  return(drv)
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

    if("description" %in% names(params)){
      new_conn$description <- params$description
    }

    if(new_conn$pool){
      new_conn$conn <- pool::dbPool(new_conn$driver(),
                                    .connection_string=new_conn$conn_str)
      reg.finalizer(new_conn$conn,pool::poolClose)

    } else {
      new_conn$conn <- DBI::dbConnect(new_conn$driver(),
                                      .connection_string=new_conn$conn_str)
    }

    assign(conn_name, new_conn, envir=connection_cache)
  },
  error=function(c){
    message(c)
    warning(glue::glue("{conn_name} is not available"))
  })
}

#' Populate the list of available connections
#'
#' @param config_filename String. Name of a config file. Defaults to NA. See
#'   \code{\link{read_config_file}} and \code{\link{read_all_configs}} for
#'   options.
#'
#' @param exclusive Boolean. Should this file be used exclusively to define
#'   connections? Defaults to \code{FALSE}. See \code{\link{read_all_configs}}.
#'
#' @details This function is called by \code{.onLoad()} and the exported
#'   \code{reconnect()} function (which closes existing connections before
#'   calling \code{set_connections()}).
create_connections <- function(.config_filename=NA, exclusive=FALSE){

  conf <- validate_all_configs(
    read_configs(config_name=.config_filename, exclusive = exclusive)
  )

  for(conn_name in names(conf)){
    add_connection(conn_name, conf[[conn_name]])
  }

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


  p <- NULL
  if(conn_name %in% names(connection_cache)){
    p <- list(
      conn = connection_cache[[conn_name]]$conn,
      runner = DBI::dbGetQuery,
      is.live = DBI::dbIsValid)
  }

  # ... Add more else if clauses here if more connections are required

  else{
    warning(glue::glue("No connection named '{conn_name}'."))
  }

  return(p)
}

#' remove a connection from the connections cache
#'
#' @param conn_name The name of the connection to be removed
prune <- function(conn_name){

  if(conn_name %in% names(connection_cache)){
    if(!connection_cache[[conn_name]]$pool){
      DBI::dbDisconnect(connection_cache[[conn_name]]$conn)
    }
    connection_cache[[conn_name]] <- NULL
    rm(list=c(conn_name), envir=connection_cache)
  }
}


#' Close all connections and remove them from the connections list
close_connections <- function(){
  lapply(names(connection_cache),prune)
}
