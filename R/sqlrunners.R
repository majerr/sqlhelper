#' Execute a sequence of SQL queries
#'
#' Accepts a character vector of SQL queries and runs each one
#'
#' @param sql An optionally-named list or character vector containing sql
#'   commands, or a tibble returned by [read_sql()]
#' @param quotesql "yes" or "no" - should interpolated characters be quoted by
#'   default?
#' @param values Should the SQL be parameterized from R? Defaults to the
#'   value of [parent.frame()]. Pass any object that is not an environment
#'   (e.g. "no" or FALSE) if interpolation is to be skipped, or another
#'   environment containing values to interpolate to avoid using
#'   [parent.frame()].
#' @param execmethod One of "get", "execute", "sendq", "sends" or "spatial" - which method should be
#'   used to execute the query? "get" means [DBI::dbGetQuery()]; "execute" means [DBI::dbExecute()]; "sendq" means
#'   \code{DBI::dbSendQuery}; "sends" means [DBI::dbSendStatement()]; "spatial" means [sf::st_read()].
#' @param geometry If \code{execmethod} is "spatial", which geometry column
#'   should be used (ignored if \code{execmethod} is not spatial)
#' @param default_conn A connection against which to execute queries if no other
#'   is specified. Either the name of a sqlhelper connection, or a database
#'   connection returned by [DBI::dbConnect()], or NA
#' @param include_params \code{TRUE} or \code{FALSE}. Should the parameters be included in the output?
#' @return If \code{include_params} is \code{FALSE}, an named list containing
#'   the results of each query; the names are the same as those in the \code{sql}
#'   parameter. If \code{include_params} is \code{TRUE}, a tibble containing the query as executed, the above parameters,
#'   and the result of each query.
#' @family SQL runners
#' @seealso \code{\link{runfiles}}
#' @export
runqueries <- function(sql,
                       quotesql = TRUE,
                       values = parent.frame(),
                       execmethod = "get",
                       geometry = NA,
                       default_conn = live_connection( get_default_conn_name() ),
                       include_params = FALSE ){

  if( ! (is(default_conn, "DBIConnection" ) | is(default_conn, "Pool")) ){

    if( is.null( connection_info() ) )
      stop("No connections are configured")

    if( ! is.character( default_conn ) )
      stop("default_conn must be a connection or the name of a connection")

    if( default_conn %in% names( connection_cache ) )
      default_conn <- live_connection( conn_name = default_conn )


    if( ! (is(default_conn, "DBIConnection" ) | is(default_conn, "Pool")) )
      stop( glue::glue("default_conn is not a connection or the name of a connection") )

  }


  prepped_sql <- prepare_sql(sql,
                              quotesql,
                              values,
                              execmethod,
                              geometry,
                              default_conn
                              )

  prepped_sql$result <- purrr::pmap(
    dplyr::select(
      prepped_sql,
      execmethod,
      conn_name,
      geometry,
      prepared_sql),

    function( execmethod, conn_name, geometry, prepared_sql, default_conn ){
      if(execmethod == "get")
        dispatcher <- DBI::dbGetQuery
      else if(execmethod == "execute")
        dispatcher <- DBI::dbExecute
      else if(execmethod == "sendq")
        dispatcher <- DBI::dbSendQuery
      else if(execmethod == "sends")
        dispatcher <- DBI::dbSendStatement
      else if(execmethod == "spatial")
        dispatcher <- sf::st_read
      else
        stop( glue::glue( "execmethod must be one of 'get', 'execute', 'sendq', 'sends' or 'spatial', not {execmethod}" ) )

      if(conn_name == "default")
        conn <- default_conn
      else
        conn <- live_connection(conn_name)

      if( execmethod == "spatial" ){
        result <- dispatcher(dsn = conn,
                             query = prepared_sql,
                             geometry_column = geometry)
      } else {
        result <- dispatcher(conn = conn,
                             statement = prepared_sql)
      }
    }, # close function definition

    default_conn = default_conn
  ) # close purrr::pmap

  execd_sql <- tibble::as_tibble(prepped_sql)

  if( include_params ){
    out <- execd_sql
  } else {
    out <- execd_sql$result
    names( out ) <- execd_sql$qname
  }

  out
}

#' Read, prepare and execute .SQL files
#'
#' Accepts a character vector of SQL file names and attempts to execute each one.
#'
#' @param filenames name, or vector of names, of file(s) to be executed
#' @param ... default parameters for [runqueries()]
#'
#' @return A list of results of sql queries found in files
#'
#' @export
runfiles <- function(filenames,
                     ...
                     # quotesql = TRUE,
                     # values = parent.frame(),
                     # execmethod = "get",
                     # geometry = NA,
                     # default_conn = live_connection( get_default_conn_name() ),
                     # include_params = FALSE
                     ){

  sql <- do.call(
    rbind,
    lapply(filenames, read_sql)
  )

  runqueries(sql, ...)
}

