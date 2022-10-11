# Add a mutable connection cache to the package namespace
assign("connection_cache",
       new.env(parent = emptyenv()),
       environment())

# Mutable env for settable defaults
assign("defaults",
       new.env(parent = emptyenv()),
       environment())

#' (Re-)Establish connections to databases
#'
#' @param .fn String, the name of a config file
#' @param exclusive if .fn is present, should it be used exclusively (TRUE) or
#'   combined with user and site configs (FALSE)?
#' Closes and then re-opens all configured connections
#'
#' @export
connect <- function(.config_filename=NA, exclusive=FALSE){
  disconnect()

  conf <- validate_configs(
    read_configs( config_name=config_name,
                  exclusive = exclusive )
  )

  for(conn_name in names(conf)){
    add_connection( conn_name,
                    conf[[conn_name]] )
  }

  set_default_conn_name( names(conf)[[1]] )
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
                                    .connection_string = new_conn$conn_str)
      reg.finalizer(new_conn$conn,pool::poolClose)

    } else {
      new_conn$conn <- DBI::dbConnect(drv(),
                                      .connection_string = new_conn$conn_str)
    }

    # connection_cache is an environment created above
    assign(conn_name, new_conn, envir = connection_cache)
  },
  error=function(c){
    message(c)
    warning(glue::glue("{conn_name} is not available"))
  })
}



#' Convert a list object returned by read_yaml() to a db
#' connection string.
#'
#' @param params Connection parameters
#'
#' @return Connection string
#'
#'   YAML values need to be double quoted in the YAML file.
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

