#' Provide a driver function
#'
#' Return a driver function appropriate for `dbtype`. If the parent package of
#' the appropriate function is not available, and the session is interactive,
#' prompt the user to install it (see `rlang::check_installed()`); an error is
#' raised if the appropriate function cannot be returned.
#'
#' @param conn_name A character string, the name of the connection needing a
#'   driver
#' @param dbtype A character string, one of 'odbc', 'sqlite', 'postgresql',
#'   'mariadb', 'mysql', or 'bigquery'.
#'
#' @return An environment containing `driver_func`, a driver function such as
#'   `odbc::odbc()`, and `driver_name`, the name of that function as a character
#'   string, e.g. "odbc::odbc".
get_driver <- function(conn_name, dbtype){
  # db_types, db_driver_pkgs, and db_driver_funcs are defined in
  # data-raw/sysdata.r

  if(!(dbtype %in% db_types))
    stop(glue::glue("Server type '{dbtype}' not recognized for connection '{conn_name}'"))

  pkgnames <- db_driver_pkgs
  names(pkgnames) <- db_types
  pn <- pkgnames[[dbtype]]

  rlang::check_installed(pn,
                         reason = glue::glue("to create connection '{conn_name}'"))

  funcnames <- db_driver_funcs
  names(funcnames) <- db_types
  fn <- funcnames[[dbtype]]

  e <- new.env(parent = emptyenv())
  e$driver_name <- glue::glue("{pn}::{fn}")
  e$driver_func <- get(fn, envir = loadNamespace(pn))
  e
}
