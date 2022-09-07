
#' Convert a list object returned by read_yaml() to a db
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
  paste0(
    paste0(
      names(params),
      "=",
      unlist(params)
    ),
    collapse="; ")
}

#' Add a new connection to the connections cache
#'
#' @param conn_name A string identifier for the new connection
#' @param params Connection parameters
#'
add_connection <- function(conn_name, params){
  tryCatch({

    # The connection template is defined in data-raw/sysdata.R and loaded via
    # R/sysdata.rda
    new_conn <- connection_template

    # Default is odbc
    if(!("server_type" %in% stringr::str_to_lower(names(params)))){
      drv <- odbc::odbc
      new_conn$driver <- "odbc::odbc"

    } else if(stringr::str_to_lower(params$server_type) == "sqlite"){
      drv <- RSQLite::SQLite
      new_conn$driver <- "RSQLite::SQLite"

    } else if(stringr::str_to_lower(params$server_type) == "postgresql"){
        drv <- RPostgres::Postgres
        new_conn$driver <- RPostgres::Postgres

    } else if(stringr::str_to_lower(params$server_type) == "mysql" |
              stringr::str_to_lower(params$server_type) == "mariadb"){
      drv <- RMariaDB::MariaDB
      new_conn$driver <- "RMariaDB::MariaDB"

    } else if(stringr::str_to_lower(params$server_type) == "bigquery"){
      drv <- bigrquery::bigquery
      new_conn$driver <- "bigrquery::bigquery"

    ### ...More patterns and drivers can be added here as needed... ###

    } else {
      drv <- odbc::odbc
      new_conn$driver <- "odbc::odbc"
    }

    if("pool" %in% names(params)){
      new_conn$pool <- params$pool
    } else {
      new_conn$pool <- FALSE
    }

    new_conn$conn_str <- yml2conn_str(params$connection)

    if("description" %in% names(params)){
      new_conn$description <- params$description
    }

    if(new_conn$pool){
      new_conn$conn <- pool::dbPool(drv(),
                                    .connection_string=new_conn$conn_str)
      reg.finalizer(new_conn$conn,pool::poolClose)

    } else {
      new_conn$conn <- DBI::dbConnect(drv(),
                                      .connection_string=new_conn$conn_str)
    }

    # connection_cache is an environment created during .onLoad()
    assign(conn_name, new_conn, envir=connection_cache)
  },
  error=function(c){
    message(c)
    warning(glue::glue("{conn_name} is not available"))
  })
}

#' Populate the list of available connections
#'
#' @param config_name String. Name of a config file. Defaults to NA. See
#'   \code{\link{read_config_file}} and \code{\link{read_all_configs}} for
#'   options.
#'
#' @param exclusive Boolean. Should this file be used exclusively to define
#'   connections? Defaults to \code{FALSE}. See \code{\link{read_all_configs}}.
#'
#' @details This function is called by \code{.onLoad()} and the exported
#'   \code{reconnect()} function (which closes existing connections before
#'   calling \code{set_connections()}).
create_connections <- function(config_name=NA, exclusive=FALSE){

  conf <- validate_configs(
    read_configs(config_name=config_name, exclusive = exclusive)
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
