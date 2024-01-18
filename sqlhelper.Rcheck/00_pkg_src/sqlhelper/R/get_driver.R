#' Provide a driver function
#'
#' Return a driver function appropriate for `dbtype`. If the parent package of
#' the appropriate function is not available, and the session is interactive,
#' prompt the user to install it (see `rlang::check_installed()`); an error is
#' raised if the appropriate function cannot be returned.
#'
#' @param conn_name A character string, the name of the connection needing a
#'   driver
#' @param drvtype A character string, one of 'odbc', 'sqlite', 'postgresql',
#'   'mariadb', 'mysql', or 'bigquery'.
#'
#' @return An environment containing `driver_func`, a driver function such as
#'   `odbc::odbc()`, and `driver_name`, the name of that function as a character
#'   string, e.g. "odbc::odbc".
#' @noRd
get_driver <- function(conn_name, drv_type){
  # driver_types, driver_pkgs, and driver_funcs are defined in
  # data-raw/sysdata.r

  if(!(drv_type %in% driver_types))
    stop(glue::glue("Driver type '{drv_type}' not recognized for connection '{conn_name}'"))

  pkgnames <- driver_pkgs
  names(pkgnames) <- driver_types
  pn <- pkgnames[[drv_type]]

  rlang::check_installed(pn,
                         reason = glue::glue("to create connection '{conn_name}'"))

  funcnames <- driver_funcs
  names(funcnames) <- driver_types
  fn <- funcnames[[drv_type]]

  e <- new.env(parent = emptyenv())
  e$driver_name <- glue::glue("{pn}::{fn}")
  e$driver_func <- get(fn, envir = loadNamespace(pn))
  e
}
