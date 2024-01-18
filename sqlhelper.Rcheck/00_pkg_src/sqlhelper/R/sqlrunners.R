#' Execute a sequence of SQL queries
#'
#' Accepts a character vector of SQL queries and runs each one
#'
#' @param sql An optionally-named list or character vector containing sql
#'   strings, or a tibble returned by [read_sql()] or [prepare_sql()]
#'
#' @param ... Arguments to be passed to `prepare_sql()`
#'
#' @param default.conn Either the name of a sqlhelper connection, or a database
#'   connection returned by [DBI::dbConnect()], or NA. This connection is only
#'   used by [glue::glue_sql()] to interpolate SQL strings, it is not used to
#'   execute any SQL code.
#'
#' @param include_params \code{TRUE} or \code{FALSE}. Should the parameters be
#'   included in the output?
#'
#' @return
#' * If \code{include_params} is \code{FALSE} and the `sql` argument is a
#' vector, a list containing the results of each query; element names will be
#' taken from the \code{sql} argument.
#' * If the length of the `sql` argument is 1 and is not named, the result of
#' that query is returned as-is (e.g. a data.frame), not as a 1-element list.
#' * If \code{include_params} is \code{TRUE}, a tibble is returned containing 1
#' row per query with the following fields:
#'
#' \describe{
#'  \item{qname}{character. A name for this query}
#'  \item{quotesql}{"yes" or "no". Should parameterized character values be quoted for this query?}
#'  \item{interpolate}{"yes" or "no". Should this query be parameterized with values from R?}
#'  \item{execmethod}{The method to execute this query.
#'  One of "get" ([DBI::dbGetQuery()]), "execute" ([DBI::dbExecute()]), "sendq" ([DBI::dbSendQuery()]), "sends" ([DBI::dbSendStatement()]) or "spatial" ([sf::st_read()])}
#'  \item{geometry}{character. If `execmethod` is "spatial", which is the geometry column?}
#'  \item{conn_name}{character. The name of the database connection to use for this query.
#'  Must be the name of a configured sqlhelper connection.}
#'  \item{sql}{The sql query to be executed}
#'  \item{filename}{The value of `file_name`}
#'  \item{prepared_sql}{The sql query to be executed, i.e. with interpolations
#'  and quoting in place}
#'  \item{result}{The result of the query}
#' }
#'
#' @examples{
#' library(sqlhelper)
#'
#' connect(
#'     system.file("examples/sqlhelper_db_conf.yml", package="sqlhelper"),
#'     exclusive=TRUE)
#'
#' DBI::dbWriteTable( default_conn(),
#'                   "iris",
#'                   iris)
#'
#' n <- 5
#'
#' runqueries(
#'     c(top_n = "select * from iris limit {n}", # interpolation is controlled
#'                                               # with the 'values' argument
#'       uniqs = "select distinct species as species from iris")
#' )
#'
#' ## Use the execmethod parameter if you don't want to return results
#' runqueries("create table iris_setosa as select * from iris where species = 'setosa'",
#'           execmethod = 'execute')
#'
#' runqueries("select distinct species as species from iris_setosa")
#' }
#' @family SQL runners
#'
#' @seealso \code{\link{runfiles}}
#'
#' @export
runqueries <- function(sql,
                       ...,
                       default.conn = default_conn(),
                       include_params = FALSE ){

  if( ! (methods::is(default.conn, "DBIConnection" ) |
         methods::is(default.conn, "Pool")) ){

    if( is.null( connection_info() ) )
      stop("No connections are configured")

    if( ! is.character( default.conn ) )
      stop("default_conn must be a connection or the name of a connection")

    if( default.conn %in% names( connection_cache ) )
      default.conn <- live_connection( conn_name = default.conn )

    if( ! (methods::is(default.conn, "DBIConnection" ) |
           methods::is(default.conn, "Pool")) )
      stop( glue::glue("default_conn is not a connection or the name of a connection") )

  }

  args <- list(...)
  args$sql <- sql
  args$default.conn <- default.conn
  if(!("values" %in% names(args))){
    args$values <- parent.frame()
  }
  prepped_sql <- do.call(prepare_sql,
                         args)

  prepped_sql$result <- purrr::pmap(
    prepped_sql[,c("execmethod", "conn_name", "geometry", "prepared_sql" )],

    function( execmethod, conn_name, geometry, prepared_sql, default.conn ){
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
        conn <- default.conn
      else
        conn <- live_connection(conn_name)

      if( execmethod == "spatial" && !is.na(geometry)){
        result <- dispatcher(dsn = conn,
                             query = prepared_sql,
                             geometry_column = geometry)
      } else if( execmethod == "spatial" ) {
        result <- dispatcher(dsn = conn,
                             query = prepared_sql)
      } else {
        result <- dispatcher(conn = conn,
                             statement = prepared_sql)
      }
    }, # close function definition

    #default.conn = parent.frame()$default.conn
    default.conn = default.conn
  ) # close purrr::pmap

  execd_sql <- tibble::as_tibble(prepped_sql)

  if( include_params ){
    out <- execd_sql
  } else {
    out <- execd_sql$result
    names( out ) <- execd_sql$qname
  }

  if(length(out) == 1 && is.na( names(out) ))
    out <- out[[1]]
  out
}

#' @rdname runqueries
#' @export
run_queries <- runqueries

#' Read, prepare and execute .SQL files
#'
#' Accepts a character vector of SQL file names and attempts to execute each one.
#'
#' @param filenames name, or vector of names, of file(s) to be executed
#' @param cascade If TRUE, fill the values of absent execution parameters with
#'   the most recent present value. This enables you to set the connection name
#'   once, for the first query in a file and use the same connection for all the
#'   subsequent queries, for example.
#' @param ... Arguments to be passed to [runqueries()]
#' @param include_params \code{TRUE} or \code{FALSE}. Should the parameters be
#'   included in the output?
#'
#' @return A list of results of sql queries found in files
#' @details
#' [runfiles()] enables you to control the arguments accepted by [runqueries()]
#' on a per-query basis, using interpreted comments in your sql file:
#'
#' ```{sql sql1, eval=FALSE}
#' -- qname = create_setosa_table
#' -- execmethod = execute
#' -- conn_name = sqlite_simple
#' CREATE TABLE iris_setosa as SELECT * FROM IRIS WHERE SPECIES = 'setosa';
#'
#' -- qname = get_setosa_table
#' -- execmethod = get
#' -- conn_name = sqlite_simple
#' SELECT * FROM iris_setosa;
#' ```
#'
#' Interpreted comments precede the sql query to which they refer. Interpretable
#' comments are:
#'
#' \describe{
#'  \item{qname}{A name for this query}
#'  \item{quotesql}{"yes" or "no" - should interpolated characters be quoted?}
#'  \item{interpolate}{"yes" or "no" - should sql be interpolated?}
#'  \item{execmethod}{One of "get", "execute", "sendq", "sends" or "spatial" -
#'   which method should be used to execute the query? "get" means
#'   [DBI::dbGetQuery()]; "execute" means [DBI::dbExecute()]; "sendq" means
#'   \code{DBI::dbSendQuery}; "sends" means [DBI::dbSendStatement()]; "spatial"
#'   means [sf::st_read()].}
#'  \item{geometry}{The name of a spatial column. Ignored if `execmethod` is not 'spatial'}
#'  \item{conn_name}{The name of a connection to execute this query against}
#' }
#'
#' All interpreted comments except `qname` are recycled within their file, meaning
#' that if you want to use the same values throughout, you need only set them for
#' the first query.
#'
#' ```{r}
#' readLines(
#'   system.file("examples/cascade.sql",
#'                 package="sqlhelper")
#' ) |> writeLines()
#'
#' ```
#'
#' @family SQL runners
#' @seealso [read_sql()], [prepare_sql()]
#' @export
runfiles <- function(filenames,
                     cascade=TRUE,
                     ...,
                     include_params = FALSE){

  sql <- do.call(
    rbind,
    lapply(filenames, read_sql, cascade = cascade)
  )

  args <- list(...)
  args$sql <- sql
  args$include_params <- include_params
  if(!("values" %in% names(args))){
    args$values <- parent.frame()
  }
  do.call(runqueries,
          args)
}

#' @rdname runfiles
#' @export
run_files <- runfiles
