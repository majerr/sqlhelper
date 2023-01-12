#' Return the combined available configurations
#'
#' See [connect()] for details
#'
#' Reads and combines configuration files for database connections. By default,
#' configuration files are sought in user- and site-level config directories,
#' and named by `config_filename`.
#'
#' @inheritParams connect
#'
#' @details Reads and combines all available config files. User- and Site-level
#'   files are sought in [rappdirs::user_config_dir()] and
#'   [rappdirs::site_config_dir()] respectively, unless `exclusive=TRUE`.
#'
#' @return optionally nested named list or vector as returned by
#'   [yaml::read_yaml()]
#'
#' @noRd
read_configs <- function(config_filename=NA, exclusive=FALSE){

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
    if(is.na(config_filename)){
      stop("A configuration filename is required when exclusive is TRUE")

      # config_filename is one of 'site' or 'user'
    } else if(config_filename %in% names(confdirs)){

      # conf_fn is defined in data-raw/sysdata.r
      fn <- file.path(confdirs[[config_filename]], conf_fn)

      # Assume config_filename is intended to be the name of a file
    } else {
      fn <- config_filename
      config_filename <- 'file'
    }

    if(!file.exists(fn)){
      stop(glue::glue("Configuration file {fn} does not exist"))
    } else {
      configs[[config_filename]] = yaml::read_yaml(fn)
    }

  # exclusive is FALSE, so search the whole config search path
  } else {
    for(conf_name in names(configs)){

      # conf_name is one of 'site' or 'user'
      if(conf_name %in% names(confdirs)){

        # conf_fn is defined in data-raw/sysdata.r as sqlhelper_db_conf.yml
        fn <- file.path(confdirs[[conf_name]],
                        conf_fn)

        if(file.exists(fn)){
          configs[[conf_name]] <- yaml::read_yaml(fn)
        }

      # conf_name is 'file' so use supplied filename (if not NA)
      } else if(!is.na(config_filename)){

        if(file.exists(config_filename)){
          configs[[conf_name]] <- yaml::read_yaml(config_filename)

        } else {
          warning(glue::glue("Configuration file '{config_filename}' does not exist"))
        }
      }

    } # end of loop
  } # end of if(exclusive = TRUE){...} else {...}

  # Drop any remaining NAs
  configs <- configs[!is.na(configs)]

  combined <- NULL

  if(length(configs) > 0){

    # Combine the configs
    combined <- configs[[1]]

    if(length(configs) > 1){
      for(conf in configs[2:length(configs)]){
        combined <- combine_configs(combined,conf)
      }
    }
  }

  combined
}

#' Combine optionally nested yaml config lists
#'
#' @param root An yml-derived config list, as returned by
#'   [yaml::read_yaml()]
#' @param new Another yml-derived config list, to be combined with root
#'
#' @return Combined yml config
#'
#' @details
#'   * Elements that exists in new but not root will be added
#'   * Elements that exists in root but not new will be retained
#'   * Elements that exist in both will be overwritten in root by the contents
#'   of new
#'
#' @noRd
combine_configs <- function(root,new){
  combined <- root

  # work through new from the bottom up
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
