#' Return the combined available configurations
#'
#' @param .config_filename String. The full name and path of a configuration file.
#' @param exclusive Boolean. If TRUE, the file named by the .config_filename parameter is
#'   treated as the only config file. Site and user level files are not read.
#'   This parameter is ignored if .fn is missing.
#' @return optionally nested named list or vector as returned by
#'   \code{yaml::read_yaml}
read_configs <- function(config_name=NA, exclusive=FALSE){

  missing_file <- FALSE

  configs <- list("site" = NA,
                  "user" = NA,
                  "file" = NA)
  # Search path for site-wide and user-specific configs
  confdirs <- list(
    "site" = rappdirs::site_config_dir(),
    "user" = rappdirs::user_config_dir()
  )

  # Read one file only
  if(exclusive == TRUE){
    if(is.na(config_name)){
      stop("A configuration filename is required when exclusive is TRUE")

      # config_name is one of 'site' or 'user'
    } else if(config_name %in% names(confdirs)){


      # conf_fn is defined in data-raw/sysdata.r
      fn <- file.path(confdirs[[config_name]], conf_fn)
      if(!file.exists(fn)){
        stop(glue::glue("Configuration file {fn} does not exist"))
      } else {
        configs[[config_name]] = yaml::read_yaml(fn)
      }

      # Assume config_name is intended to be the name of a file
    } else {
      if(!file.exists(config_name)){
        stop(glue::glue("Configuration file {config_name} does not exist"))
      }
      configs[["file"]] <- yaml::read_yaml(config_name)
    }

  # exclusive is FALSE, so search the whole config search path
  } else {
    for(conf_name in names(configs)){

      # conf_name is one of 'site' or 'user'
      if(conf_name %in% names(confdirs)){

        # conf_fn is defined in data-raw/sysdata.r
        fn <- file.path(confdirs[[conf_name]],
                        conf_fn)

        if(file.exists(fn)){
          configs[[conf_name]] <- yaml::read_yaml(fn)
        }

      # conf_name is 'file' so use supplied filename (if not NA)
      } else if(!is.na(config_name)){

        if(file.exists(config_name)){
          configs[[conf_name]] <- yaml::read_yaml(config_name)

        } else if(!(config_name %in% names(confdirs))){
          missing_file <- TRUE
        }
      }
    }
  }

  # Drop any remaining NAs
  configs <- configs[!is.na(configs)]

  # Error if no configs
  if(length(configs) == 0){
    stop("No configuration files found")

  # Warn if the specified file is missing
  } else if(missing_file){
    warning(glue::glue("Configuration file '{config_name}' does not exist"))
  }

  # Combine the configs
  combined <- configs[[1]]

  if(length(configs) > 1){
    for(conf in configs[2:length(configs)]){
      combined <- combine_configs(combined,conf)
    }
  }

  combined
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


