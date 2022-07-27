# configure.R - internal functions related to configuration

#' Return the contents of a config file
#'
#' @param conf String. Either the path and name of a config file, or
#'   'site', or 'user', or \code{NA} (the default). If \code{NA},
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
    # conf is a filename
    fn <- as.character(conf)
  } else if(!is.null(confdirs[[conf]])) {
    # conf is one of 'site' or 'user'
    path <- confdirs[[conf]]()
  } else {
    # assume conf is the current directory
    path <- getwd()
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

#' Combine optionally nested yaml config lists
#'
#' @param root An yml-derived config list, as returned by
#'   [read_config_file]
#' @param new Another yml-derived config list, to be inserted
#'
#' @return Combined yml config
#'
#' @details
#'   * Elements that exists in new but not root will be added
#'   * Elements that exists in root but not new will be retained
#'   * Elements that exist in both will be replaced in root by the contents of new
#'
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
      # both lists (i.e. branches in the tree), so we need to recurse into them.
      combined[[name]] <- combine_configs(root[[name]],new[[name]])
    }
  }
  combined
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

#' Perform minimal sanity checks on a configuration
#'
#' @param conf a single connection configuration returned by [get_all_configs]
#'
#' @return A valid connection configuration, or NULL
#'
#' @details \code{validate_conf} attempts to ensure that connection
#'   configurations contain at least the elements \code{c(pool=boolean(),
#'   connection$Server=character())}, and that all child elements of
#'   \code{connection} are character vectors or length 1.
#'
#'   If the \code{pool} element is not present, or not a logical of length 1, it
#'   is inserted with the default value (FALSE).
#'
#'   If the \code{connection} element does not contain a \code{server} child
#'   element, or a child element of \code{connection} is not a character vector
#'   of length 1, NULL is returned.
validate_config <- function(conf){
  # conf is a list
  if(!is.list(conf)){
    return(NULL)
  }

  # conf contains a connection element
  if(!("connection" %in% names(conf))){
    return(NULL)
  }

  # all the children of conf$connection are strings of length 1
  for(name in names(conf$connection)){
    if(!(is.character(conf$connection[[name]])) |
       length(conf$connection[[name]]) != 1){
      return(NULL)
    }
    conf$connection[[name]] <- stringr::str_to_title(conf$connection[[name]])
  }

  if(!("Server" %in% names(conf$connection))){
    return(NULL)
  }

  valid_conf <- conf
  # conf has a pool element that is a logical and length 1
  if(!("pool" %in% names(valid_conf))){
    valid_conf$pool <- FALSE
  } else if(!is.logical(valid_conf$pool) |
            length(valid_conf$pool) != 1){

    valid_conf$pool <- FALSE
  }

 valid_conf
}

#' Apply validate_config to all connection configurations
#'
#' @param configs a configuration object returned by [get_all_configs]
#'
#' @return A list of valid configs, similar to one returned by [get_all_configs]
#'
#' @details Each configuration is passed to [validate_config]; unfixable configs
#'   are dropped with a warning
validate_all_configs <- function(configs){
  validated_configs <- lapply(configs, validate_config)

  for(confname in names(validated_configs)){

    if(is.null(validated_configs[[confname]]) & interactive()){
      warning(glue::glue("Connection configuration for {confname} was invalid; {confname} will not be available"))
    }

  }

  validated_configs[!is.null(validated_configs)]
}


