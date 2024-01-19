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
#' If `exclusive=FALSE` (the default), configuration files will be sought in the
#' directory returned by [rappdirs::site_config_dir()], the directory returned
#' by [rappdirs::user_config_dir()], and finally a file named by
#' `config_filename` (if not `NA`). If elements of those files conflict, later
#' files overwrite the elements of earlier files.
#'
#' If `exclusive=TRUE`, only 1 file, indicated by the
#' `config_filename` parameter,  will be read.
#'
#'   * If `config_filename = "site"`, a config file called
#'   `sqlhelper_db_conf.yml` will be sought in the directory returned by
#'   [rappdirs::site_config_dir()]
#'   * If `config_filename = "user"`, a config file called
#'   `sqlhelper_db_conf.yml` will be sought in the directory returned by
#'   [rappdirs::user_config_dir()]
#'   * If `config_filename` is not `NULL` (but not "site" or "user"), it is
#'   assumed to name a file.
#'
#' A warning is raised if no valid configurations are found (e.g. `connect()` is
#' called without arguments and no site- or user-wide files are present, or the
#' connections in those files are invalid)
#'
#' `vignette("connections")` explains how to write a
#' config file and how to access the created connections.
#'
#' @param config_filename String. The full name and path of a configuration
#'   file, or "site", or "user", or "example", or `NA` (the default). Cannot be
#'   `NA` if `exclusive = TRUE`.
#' @param exclusive Logical. If `TRUE`, the file named by `config_filename` is
#'   treated as the only config file. Site and user level files are not read.
#'   This parameter is ignored if `config_filename` is missing.
#'
#' @returns `NULL`, invisibly
#' @export
#' @examples
#' library(sqlhelper)
#'
#' example_filename <- system.file("examples",
#'                                 "sqlhelper_db_conf.yml",
#'                                 package = "sqlhelper")
#'
#' # Search for config files in rappdirs::site_config_dir(),
#' # rappdirs::user_config_dir(), and read from example_filename
#' connect(example_filename)
#'
#' # Read only the named example file
#' connect(example_filename, exclusive=TRUE)
#'
connect <- function(config_filename=NA, exclusive=FALSE){
  disconnect()

  conf <- read_configs( config_filename,
                        exclusive ) |>
    validate_configs()

  if( length(conf) ){

    for(conn_name in names(conf) ){
      add_connection( conn_name,
                      conf[[conn_name]] )
    }
  } else {
    warning("No connections were configured")
  }

  if( length( names( connection_cache )))
    set_default_conn_name( names(conf)[[1]] )

  invisible(NULL)
}

#' Add a new connection to the connections cache
#'
#' @param conn_name A string identifier for the new connection
#' @param params Connection parameters read by [read_configs]
#' @returns NULL
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

    # connection_cache is an environment created at the top of this file
    assign(conn_name, new_conn, envir = connection_cache)
  },
  error=function(c){
    message(glue::glue("Error whilst connecting {conn_name}:
                       {c}"))
    warning(glue::glue("{conn_name} is not available"))
  })

  NULL
}
