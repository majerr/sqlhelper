#' Apply validate_config to all connection configurations
#'
#' @param configs a configuration object returned by [read_all_configs]
#'
#' @return A list of valid configs, similar to one returned by [read_configs]
#'
#' @details Each configuration is passed to [validate_config]; invalid configs
#'   are dropped with a warning
#' @noRd
validate_configs <- function(configs){

  if( length(configs) == 0){
    return(NULL)
  }

  validated_configs <- lapply(configs, validate_config)

  for(confname in names(validated_configs)){

    if(is.null(validated_configs[[confname]]) & interactive()){
      warning(glue::glue("Connection configuration for {confname} was invalid; {confname} will not be available"))
    }

  }

  validated_configs[!is.null(validated_configs)]
}

#' Perform minimal sanity checks on a configuration
#'
#'  `validate_conf` attempts to ensure that connection configurations contain at
#'  least the elements `c(pool=logical(), connection$Server=character())`,
#'  and that all child elements of \code{connection} are character vectors of
#'  length 1.
#'
#'  If the \code{pool} element is not present, or not a logical of length 1, it
#'  is inserted with the default value `FALSE`.
#'
#'  If the \code{connection} element does not contain a \code{server} element,
#'  or any child of \code{connection} is not a character vector of length 1,
#'  NULL is returned.
#' @param conf a single connection configuration returned by [read_configs]
#'
#' @return A valid connection configuration, or NULL
#' @noRd
validate_config <- function(conf){

  # conf is a list
  if(!is.list(conf)){
    return(NULL)
  }

  # conf contains a server_type element
  if(!("server_type" %in% names(conf))){
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
      print(length(conf$connection[[name]]))
      return(NULL)
    }
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
