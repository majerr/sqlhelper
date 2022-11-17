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
#' Closes any open connections, reads config files as directed by
#' `config_filename` and `exclusive`, and creates new connections from the
#' descriptions in those files.
#'
#' See also `vignette("Managing connections", pkg="sqlhelper)` for details of these
#' operations
#'
#'
#' @param config_filename String. The full name and path of a configuration
#'   file, or `NA` (the default). Cannot be `NA` if `exclusive = TRUE`.
#' @param exclusive Logical. If `TRUE`, the file named by `config_filename` is
#'   treated as the only config file. Site and user level files are not read.
#'   This parameter is ignored if `config_filename` is missing.
#'
#'
#' @export
#' @examples
#'
#' # Search for config files in rappdirs::site_config_dir() and
#' # rappdirs::user_config_dir()
#' connect()
#'
#' \dontrun{
#' # Search for config files in rappdirs::site_config_dir(),
#' # rappdirs::user_config_dir(), and read `my_connections.yml` in the current
#' # working directory
#' connect("my_connections.yml")
#'
#' # Read only `my_connections.yml` in the current working directory
#' connect("my_connections.yml", exclusive=TRUE)
#' }
connect <- function(config_filename=NA, exclusive=FALSE){
  disconnect()

  conf <- validate_configs(
    read_configs( config_filename,
                  exclusive )
  )

  if( length(conf) ){

    for(conn_name in names(conf) ){
      add_connection( conn_name,
                      conf[[conn_name]] )
    }
  }

  if( length( names( connection_cache )))
    set_default_conn_name( names(conf)[[1]] )

}

#' Add a new connection to the connections cache
#'
#' @param conn_name A string identifier for the new connection
#' @param params Connection parameters read by [read_configs]
#'
#' @noRd
add_connection <- function(conn_name, params){
  tryCatch({

    # The connection template is defined in data-raw/sysdata.R and loaded via
    # R/sysdata.rda
    new_conn <- connection_template

    driver <- get_driver(conn_name, stringr::str_to_lower(params$driver_type))
    new_conn$driver <- driver$driver_name
    drv <- driver$driver_func

    if("pool" %in% names(params)){
      new_conn$pool <- params$pool
    } else {
      new_conn$pool <- FALSE
    }

    if("description" %in% names(params)){
      new_conn$description <- params$description
    }

    if(new_conn$pool){
      new_conn$conn <- do.call( pool::dbPool,
                                c(drv(),
                                  params$connection)
                              )

      reg.finalizer( new_conn$conn,
                     pool::poolClose )

    } else {
      new_conn$conn <- do.call( DBI::dbConnect,
                                c(drv(),
                                  params$connection)
                              )
    }

    # connection_cache is an environment created above
    assign(conn_name, new_conn, envir = connection_cache)
  },
  error=function(c){
    message(glue::glue("Error whilst connecting {conn_name}:
                       {c}"))
    warning(glue::glue("{conn_name} is not available"))
  })
}
