# This file contains several (mainly) internal functions used either by exported
# functions (e.g. those in defined in sqlrunners.R) or called by .onLoad()

# yml2str converts a list object returned by read_yaml() to a connection string.
#
# YAML values need to be double quoted in the YAML file.
#
# For internal use.
yml2str <- function(.conparms){
  paste0(
    paste0(
      names(.conparms),
      "=",
      unlist(.conparms)
      ),
    collapse="; ")
}

# get_conf_filename identifies the right conf file to use
#
# 4 possible files are listed, the first one that is found is used.
# A filename supplied as an arg is preferred, then a conf file in
# the pwd, then in the users config dir, as identified by rappdirs,
# then in the site config dir, also identified by rappdirs.
get_conf_filename <- function(.fn=NA){
  basefn <- ".sqlhelper_db_conf.yml"
  file_names <- c( # list of possible conf filenames
      .fn, # arg
      file.path(
        getwd(),
        basefn),
      file.path(
        rappdirs::user_config_dir(),
        basefn),
      file.path(
        rappdirs::site_config_dir(),
        basefn)
      )

  existing_file_names <- na.omit( # list of existing conf filenames
    unlist(
      lapply(file_names,function(.fn){
        if(is.na(.fn)){
            NA
          } else if(!file.exists(.fn)) { # This needs a seperate clause to avoid 'invalid arg' errors
            NA
          } else {
            .fn
          }
      })
    )
  )
  dplyr::first(existing_file_names)
}

# set_connections() is called by .onLoad()
#
# Connections is a predefined list, loaded from R/sysdata.rda, with one element:
#
# > names(connections) [1] "cds"
#
# set_connections() sets cds to an open ODBC connection.
#
# For version 3, this will change to user or site defineable connections.
#
# To add more in the future, first add the name of the new connection in
# R/sysdata.rda and then specify the connection in this function
set_connections <- function(config_filename=NA){

  fn <- get_conf_filename(.fn=config_filename)
  conf <- yaml::read_yaml(fn)
  con_strs <- lapply(conf,yml2str)

  default_conn_name <<- names(con_strs)[1]
  for(con_name in names(con_strs)){
    tryCatch({
      suppressWarnings({
        connections[[con_name]] <<- DBI::dbConnect(odbc::odbc(),
                                                  .connection_string=con_strs[[con_name]])
      })

    },
    error = function(c){
      connections[[con_name]] <- NA
      warning(glue::glue("{con_name} is not available"))
    })
  }
}

#

# Returns a connection and a sql runnner for the db parameter.
# For internal use only!
#
# For now, qlhelpr only uses DBI, so this isn't really necessary,
# but if in the future we need runners or is.live functions from
# other packages we can add them here without needing to mess about
# with the sqlrunners code.
getrunparams <- function(db){
  if(db %in% names(connections)){
    return(
      list(
        conn = connections[[db]],
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
#' @param db Which database do you want to test? (restricted to 'cds' in MHCLG)
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
is.connected <- function(db){
  runparms <- getrunparams(db)
  out <- NA
  tryCatch({out <- runparms$is.live(runparms$conn)},
           error = function(e){
             out<<-TRUE
           })
  return(out)
}

#' Test whether a database is connected
#'
#' @param db Which database do you want to test? (restricted to 'cds' in MHCLG)
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
not.connected <- function(db){
  runparms <- getrunparams(db)
  out <- NA
  tryCatch({out <- runparms$is.live(runparms$conn)},
           error = function(e){
           print(runparms$is.live(runparms$conn))
             out<<-FALSE
           })
  return(!out)
}

#' Close connections to Hive and PostgreSQL
#'
#' This function is run when the library is unloaded
close_connections <- function(){
  for(conn_name in names(connections)){
    if(is.connected(conn_name)){
      suppressWarnings(DBI::dbDisconnect(connections[[conn_name]]))
    }
    connections[[conn_name]] <- NA
  }
}

#' Re-establish connections to all configured databases
#'
#' Closes and then re-opens connections all configured connections
#'
#' @export
reconnect <- function(.fn=NA){
  close_connections()
  set_connections(.fn)
}

#' Return a list of available connection names
#'
#' @return A list of connections maintained by the loaded sqlhelper that are
#'   currently live.
#'
#' @export
connections_list <- function(){
  live_cons <- lapply(names(connections),is.connected)
  return(names(connections)[live_cons != FALSE])
}

#' Return the named connection or NULL
#'
#' @param con_name Then name of the live connection you want (use
#'   connections_list to get names of available connetions)
#'
#' @return A live connection to a database, or NULL
#'
#'   If \code{con_name} is not the name of a live connection to a database, a
#'   warning is issued and NULL is returned.
#'
#' @export
live_connection <- function(con_name) {
  live_cons <- lapply(names(connections),is.connected)
  if (con_name %in% names(connections)[live_cons != FALSE]) {
    return(connections[[con_name]])

  } else {
    warning(glue::glue("There is no live connection named '{con_name}'"))
    return(NULL)
  }
}
