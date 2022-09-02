#' Return the combined available configurations
#'
#' @param .config_filename String. The full name and path of a configuration file.
#' @param exclusive Boolean. If TRUE, the file named by the .config_filename parameter is
#'   treated as the only config file. Site and user level files are not read.
#'   This parameter is ignored if .fn is missing.
#' @return optionally nested named list or vector as returned by
#'   \code{yaml::read_yaml}
read_all_configs <- function(.config_filename=NA, exclusive=FALSE){
  ## If it's just the named file, we can take a shortcut
  if(!is.na(.config_filename) && exclusive){
    return(read_config_file(.config_filename))
  }

  all_configs <- list(
    "site" = read_config_file("site"),
    "user" = read_config_file("user"),
    "pwd"  = read_config_file(),
    "fn"   = read_config_file(.config_filename)
  )

  # Warn if 'site' or 'user' were specified explicitly but those files don't exist
  if(!is.na(.config_filename)){
    if(.config_filename == "site" && is.na(all_configs$site)) {
      # site-wide specified but doesn't exist
      warning(glue::glue("No file named '{conf_fn}' was found in {rappdirs::site_config_dir()}"))

    } else if(.config_filename == "user" && is.na(all_configs$user)){
      # user-specific specified but doesn't exist
      warning(glue::glue("No file named '{conf_fn}' was found in {rappdirs::site_config_dir()}"))
    }
  }

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

#' Return the contents of a config file
#'
#' @param conf String. Either the path and name of a config file, or
#'   'site', or 'user', or \code{NA} (the default). If \code{NA},
#'   \code{read_config_file} will search the current directory for a config file.
#'
#' @details The default search name for config files is "sqlhelper_db_conf.yml".
#'   If \code{conf} is 'site', 'user' or \code{NA}, a file with this name will
#'   be sought in \code{rappdirs::site_config_dir},
#'   \code{rappdirs::user_config_dir} or the current working directory,
#'   respectively. In an interactive context, a warning will be issued if the
#'   sought file does not exist.
#'
#' @return optionally nested named list or vector as returned by
#'   \code{yaml::read_yaml}, or \code{NA} if the indicated file does not
#'   exist.
read_config_file <- function(conf=NA){
  confdirs <- list(
    "site" = rappdirs::site_config_dir,
    "user" = rappdirs::user_config_dir
  )

  path <- NA
  fn <- NA
  if(file.exists(as.character(conf))) {
    # conf is a filename
    fn <- as.character(conf)
  } else if(!is.null(confdirs[[conf]])) {
    # conf is one of 'site' or 'user'
    path <- confdirs[[conf]]()
  } else if(!is.na(conf)){
    # conf is a string but is not the name of a file or 'site' or 'user'
    warning(glue::glue("{conf} is not a filename or 'site' or 'user'"))
  }

  if(is.na(path)) {
    # assume conf file is in the current directory
    path <- getwd()
  }

  if(is.na(fn)){
    fn <- file.path(path,
                    conf_fn) # conf_fn is defined in data-raw/sysdata.r
  }

  fexists <- file.exists(fn)

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


