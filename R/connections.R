# This file contains several (mainly) internal functions used either by exported
# functions (e.g. those in defined in sqlrunners.R) or called by .onLoad()

#' yml2conn_str converts a list object returned by read_yaml() to a db
#' connection string.
#'
#' @param parms Connection parameters
#'
#' @return Connection string
#'
#'   YAML values need to be double quoted in the YAML file.
#'
#'   For internal use.
yml2conn_str <- function(parms){
  conparms <- parms$connection
  paste0(
    paste0(
      names(conparms),
      "=",
      unlist(conparms)
      ),
    collapse="; ")
}

#' Return the contents of a config file
#'
#' @param conf String. Either the path and name of a config file, or one
#'   of 'site' or 'user', or \code{NA} (the default). If \code{NA},
#'   \code{get_config} will search the current directory for a config file.
#'
#' @details The default search name for config files is "sqlhelper_db_conf.yml".
#'   If \code{conf} is 'site', 'user' or \code{NA}, a file with this name will
#'   be sought in \code{rappdirs::site_config_dir},
#'   \code{rappdirs::user_config_dir} or the current working directory,
#'   respectively. In an interactive context, a warning will be issued if the
#'   sought file does not exist.
#'
#' @return optionally nested named list or vector as returned by
#'   \code{yaml::read_yaml}, or \code{NA} if the appropriate file does not
#'   exist.
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

  if(interactive() & !fexists){
      message(glue::glue("{fn} does not exist"))
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

  if(length(all_configs) == 0){
    combined = NA
  } else {
    combined <- all_configs[[1]]
    for(conf in all_configs[2:length(all_configs)]){
      combined <- combine_configs(combined,conf)
    }
  }

  return(combined)
}

#' Combine the optionally nested lists root and new.
#'
#' @param root The existing yml config
#' @param new The yml to be added
#'
#' @return Combined yml config
#'
#' Elements that exists in new but not root will be added to root
#' Elements that exists in root but not new will be retained in root
#' Elements that exist in both will be replaced in root by the contents of new
combine_configs <- function(root,new){
  combined <- root
  for(name in rev(names(new))){
    if(!(name %in% names(root))) {
      # New name does not yet exist in the combined list- insert it at the first
      # position to enable use of the first as default.
      existing_names <- names(combined)
      combined <- c(new[name], combined)
      names(combined) <- c(name, existing_names)

    } else if(!is.list(root[[name]])){
      # New name already exists but the existing element is not a list (i.e. it
      # is a leaf in the tree) - so overwrite
      combined[[name]] <- new[[name]]

    } else if(!is.list(new[[name]]))
      # New name already exists but the new element is not a list (i.e. when
      # inserted it will be a leaf in the tree) - so overwrite
      combined[[name]] <- new[[name]]

    else {
      # New name already exists and the existing and replacement elements are
      # both lists, so we need to recurse into them.
      combined[[name]] <- combine_configs(root[[name]],new[[name]])
    }
  }
  combined
}

#' Determine the connection driver
#'
#' @param conf A named list representing a single connection returned by
#'   \code{\link{get_all_configs}}.
#'
#' @details Search for an element named "database_type" and return a driver
#'   function appropriate for that database. For example, "database_type:
#'   sqlite" in your config will case \code{driver()} to return
#'   \code{\link[RSQLite]{SQLite}}.
#' @return A driver function. Defaults to \code{\link[odbc]{odbc}} if no
#'   "database_type" element is found.
#' @import dplyr RSQLite odbc stringr
driver <- function(conf){
  # Default is odbc
  if(!("database_type" %in% stringr::str_to_lower(names(conf)))){
    drv <- odbc::odbc
  } else {

    drv <- dplyr::case_when(
      stringr::str_to_lower(conf$database_type) == "sqlite" ~ list(RSQLite::SQLite),

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
#' @Details This function is called by \code{.onLoad()} and may be re-called
set_connections <- function(config_filename=NA, exclusive=FALSE){

  conf <- get_all_configs(.fn=config_filename, exclusive = exclusive)

  con_strs <- lapply(conf, yml2conn_str)

  default_conn_name <<- names(con_strs)[1]
  for(con_name in names(con_strs)){
    drv <- driver(conf[con_name][[1]])
    print(con_strs[con_name])
    print(str(drv()))
    tryCatch({
     suppressWarnings({
       if(conf[con_name][[1]]$pool){
         connections[[con_name]] <<- pool::dbPool(drv(),
                                                   .connection_string=con_strs[[con_name]])
       } else {
         print("!")
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


# Returns a connection and a sql runnner for the db parameter.
# For internal use only!
#
# For now, sqlhelpr only uses DBI, so this isn't really necessary,
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
is.connected <- function(cname){
  runparms <- getrunparams(cname)
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

#' Close all connections and remove them from the connections list
#'
#' This function is run when the library is unloaded
close_connections <- function(){
  for(conn_name in names(connections)){
    if(is.connected(conn_name) & !is(live_connection(conn_name),"Pool")){
      suppressWarnings(DBI::dbDisconnect(connections[[conn_name]]))
    } else if(is.connected(conn_name) & is(live_connection(conn_name),"Pool")){
      pool::poolClose(connections[[conn_name]])
    }
    connections[[conn_name]] <<- NA
  }
  connections <<- list()
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
#' @param con_name The name of the live connection you want (use
#'   connections_list to get names of available connections)
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
