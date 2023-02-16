#' Execute a sequence of SQL queries
#'
#' Accepts a character vector of SQL queries and runs each one
#'
#' @param sql An optionally-named list or character vector containing sql
#'   commands, or a tibble returned by [read_sql()]
#' @param quotesql "yes" or "no" - should interpolated characters be quoted by
#'   default?
#' @param interpolate Should the SQL be parameterized from R? Defaults to the
#'   value of [parent.frame()]. Pass any object that is not an environment
#'   (e.g. "no" or FALSE) if interpolation is to be skipped, or another
#'   environment containing values to interpolate to avoid using
#'   \code{.GlobalEnv}.
#' @param execmethod One of "get", "execute", "sendq", "sends" or "spatial" - which method should be
#'   used to execute the query? "get" means [DBI::dbGetQuery()]; "execute" means [DBI::dbExecute()]; "sendq" means
#'   \code{DBI::dbSendQuery}; "sends" means [DBI::dbSendStatement()]; "spatial" means [sf::st_read()].
#' @param geometry If \code{execmethod} is "spatial", which geometry column
#'   should be used (ignored if \code{execmethod} is not spatial)
#' @param conn Either the name of a sqlhelper connection, or a database
#'   connection returned by [DBI::dbConnect()], or NA
#' @param include_params Should the parameters be included in the output?
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

#' Execute a list of .sql files
#'
#' Accepts a character vector of SQL file names and runs each one on either hive
#' or postgresql.
#'
#' @param filenames A character vector containing your sql file names, including
#'   paths if they are not in the working directory
#' @param db Defaults to 'cds' which is the only option on the DAP
#'   Although you \emph{can} specify `db` as a parameter, the preferred way is
#'   to include a `-- db=<dbname>` comment in your sql file. See details.
#' @param interpolate defaults to the value of \code{parent.frame()}. May be set
#'   to \code{FALSE} if interpolation to be avoided, or to another environment
#'   to control the source of the used. This can be useful if \code{sqlhelper} is
#'   used to run SQL from within another package, and you don't want to
#'   interfere with \code{.GlobalEnv}. See details.
#' @details Before queries are sent to the database for execution,
#'   \code{runfiles} interprets comments and parameterizes queries.
#'
#'   \strong{Comment interpretation:}
#'
#'   \code{sqlhelper} can extract parameters from two kinds of SQL comments. Both
#'   must occur on a line by themselves.
#'
#'   \describe{
#'
#'   \item{\code{-- db=cds}}{The \code{db} parameter is legacy from previous versions of the package,
#'   where multiple databases were available.
#'   For use on the DAP, 'cds' is the only accepted value. Case insensitive. Use this
#'   once before the first query in the file. Don't quote the parameter! For example,
#'   use \code{-- db=cds}, NOT \code{-- db="cds"}.}
#'
#'   \item{\code{-- qname=<a_name_for_this_query>}}{The \code{qname} parameter
#'   assigns a name to each query, which can then be used to access the result
#'   of that query. Don't quote the parameter! For example,
#'   if your query is called create_tab, use \code{-- qname=create_tab}, NOT \code{-- qname="create_tab"}.}
#'
#'   }
#'
#'   \strong{Query parameterization:}
#'
#'   Queries may be parameterized with values from the R. In fact you may run
#'   arbitrary R code to parameterize your queries. Parameters are enclosed in
#'   braces (\code{{...}}). So if you have defined a parameter \code{max_rows}
#'   in R:
#'
#'   \code{max_rows <- 10}
#'
#'   You might use it in your SQLServer code as part of a TOP clause to get 10 rows of a
#'   table:
#'
#'   \code{SELECT TOP {max_rows} * FROM mytable}
#'
#'   Before your query is submitted to the server, it will be parameterized to:
#'
#'   \code{SELECT TOP 10 * FROM mytable}
#'
#'   By default, parameters are searched for from the package up to the global
#'   environment, so if you have defined your parameters globally, they will be
#'   found. You might not always want this though. For example, if you are
#'   calling \code{runfiles} to run parameterized SQL from within a package, you
#'   shouldn't interfere with your user's global environment. In this case you
#'   will want to pass in an environment containing the required parameters. In this
#'   case use the \code{interpolate} argument:
#'
#'   \preformatted{
#'   >param_env <- new.env()
#'   >param_env$max_rows <- 10
#'   >runfiles("my_queries.sql", interpolate=param_env)
#'   }
#'
#'   If you want to avoid parameterization from R altogether, set the
#'   \code{interpolate} parameter to \code{FALSE}.
#'
#' @return Depending on the input, \code{runfiles} returns one of:
#'
#'   \describe{
#'
#'   \item{\strong{A list of lists} (for multiple files)}{The outer list
#'   contains one list for each file in filenames. Each inner list is named with
#'   the file basename and contains the results of each query from that file.}
#'
#'   \item{\strong{A list} (for a single file containing several queries)}{Each
#'   element is the result of a query.}
#'
#'   \item{\strong{An R object} (for files containing a single query).}{The
#'   result of the query, e.g. a dataframe for a \code{SELECT} statement, or a
#'   single element character vector for a \code{CREATE TABLE} statement.} }
#'
#'   \code{runfiles()} will try to flatten the list intelligently so that you
#'   don't have to name lots of containers that contain only one element to
#'   inspect your results. But this may mean that you get a dataframe where you
#'   were expecting a list, e.g. if your file contains only one query.
#'
#' @family SQL runners
#' @export
runfiles <- function(filenames,
                     quotesql = TRUE,
                     values = parent.frame(),
                     execmethod = "get",
                     geometry = NA,
                     default_conn = live_connection( get_default_conn_name() ),
                     include_params = FALSE){

  sql <- purrr::map_dfc(filenames, read_sql)

  print(names(sql))

  runqueries(sql,
             quotesql,
             values,
             execmethod,
             geometry,
             default_conn,
             include_params)
}

