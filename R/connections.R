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

#' Return the contents of a config file
#'
#' @param conf A string. either the full path and name of a config file, or one
#'   of 'site' or 'user', or \code{NA} (the default). If \code{NA},
#'   \code{get_config} will search the current directory for a config file.
#'
#' @return optionally nested named list or vector as returned by
#'   \code{yaml::read_yaml}
#'
#' @export
read_config_file <- function(conf=NA){
  confdirs <- list(
    "site" = rappdirs::site_config_dir,
    "user" = rappdirs::user_config_dir
  )

  fn <- NA
  if(file.exists(as.character(conf))) {
    fn <- as.character(conf)
  } else if(!is.null(confdirs[[conf]])) {
    path <- confdirs[[conf]]()
  } else {
    path <- "./"
  }
  if(is.na(fn)){
    fn <- file.path(path,
                    conf_fn) # conf_fn is defined in data-raw/sysdata.r
  }
  fexists <- file.exists(fn)

  if(interactive()){
    if(fexists){
      msg <- fn
    } else {
      msg <- glue::glue("{fn} does not exist")
    }
    message(msg)
  }

  if(fexists){
    yml <- yaml::read_yaml(fn)
  } else {
    yml <- NA
  }
  yml
}

#' Return the combined available configurations
#'
#' @param .fn String. The full name and path of a configuration file.
#' @param exclusive Boolean. If TRUE, the file named by the .fn parameter is
#'   treated as the only config file. Site and user level files are not read.
#'   This parameter is ignored if .fn is missing.
#' @return optionally nested named list or vector as returned by
#'   \code{yaml::read_yaml}
get_all_configs <- function(.fn=NA,exclusive=FALSE){
  ## If it's just the named file, we can take a shortcut
  if(!is.na(.fn) && exclusive){
    return(read_config_file(.fn))
  }

  all_configs <- list(
    "site" = read_config_file("site"),
    "user" = read_config_file("user"),
    "pwd"  = read_config_file(),
    "fn"   = read_config_file(.fn)
  )
  all_configs <- all_configs[!is.na(all_configs)]

  combined <- all_configs[[1]]
  for(conf in all_configs[2:length(all_configs)]){
    combined <- combine_configs(combined,conf)
  }
  combined
}

#' Combine the optionally nested lists root and new.
#' Elements that exists in new but not root will be added to root
#' Elements that exists in root but not new will be retained in root
#' Elements that exist in both will be replaced in root by the contents of new
combine_configs <- function(root,new){
  combined <- root
  for(name in names(new)){
    if(!(name %in% names(root))) {
      combined[[name]] <- new[[name]]
    } else if(!is.list(root[[name]])){
      combined[[name]] <- new[[name]]
    } else if(!is.list(new[[name]]))
      combined[[name]] <- new[[name]]
    else {
      combined[[name]] <- combine_configs(root[[name]],new[[name]])
    }
  }
  combined
}

# get_conf_filenames returns the names of configuration files to be read
#
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
          } else if(!file.exists(.fn)) { # This needs a separate clause to avoid 'invalid arg' errors
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
set_connections <- function(config_filename=NA, exclusive=FALSE){

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
#' Closes and then re-opens all configured connections
#'
#' @export
reconnect <- function(.fn=NA,exclusive=NA){
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
