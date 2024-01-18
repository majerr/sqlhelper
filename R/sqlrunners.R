#' Execute a sequence of SQL queries
#'
#' Accepts a character vector of SQL queries and attempts to execute each
#'
#' @param sql An optionally-named list or character vector containing sql
#'   strings, or a tibble returned by [read_sql()] or [prepare_sql()].
#'
#' @param ... Arguments to be passed to [read_sql()] or `prepare_sql()`
#'
#' @param default.conn Either the name of a sqlhelper connection, or a database
#'   connection returned by [DBI::dbConnect()] or [pool::dbPool()]. This
#'   connection is used as a fall-back when the `sql` parameter is a tibble and
#'   no per-query connection name is supplied, or the connection name is
#'   `default` (see [prepare_sql()]). It may be used by [glue::glue_sql()] to
#'   interpolate SQL strings, and as the connection against which to execute SQL
#'   queries.
#'
#' @param include_params \code{TRUE} or \code{FALSE}. Should the parameters be
#'   included in the output? Mainly useful for debugging.
#'
#' @details If no default connection is supplied via `default.conn` and no
#'   connections have been configured using `connect()`, an attempt will be made
#'   to configure connections via `connect()` using the configuration search
#'   path. If no database connections are available after this attempt, an error
#'   will be raised. See `vignette("connections")` for details about the
#'   configuration search path.
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
#'  \item{geometry}{character. If `execmethod` is "spatial", this should be the name of the geometry column.}
#'  \item{conn_name}{character. The name of the database connection against which to execute this query.
#'  Must be the name of a configured sqlhelper connection.}
#'  \item{sql}{The sql query to be executed}
#'  \item{filename}{The value of `file_name`}
#'  \item{prepared_sql}{The sql query to be executed, i.e. with interpolations
#'  and quoting in place}
#'  \item{result}{The result of the query}
#' }
#'
#' @examples
#' library(sqlhelper)
#'
#' readLines(
#'     system.file("examples/sqlhelper_db_conf.yml",
#'                 package="sqlhelper")
#'     ) |>
#' writeLines()
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
#' run_queries(
#'     c(top_n = "select * from iris limit {n}",
#'       uniqs = "select distinct species as species from iris")
#' )
#'
#' ## use include_params to review the execution context
#' run_queries(
#'     c(top_n = "select * from iris limit {n}",
#'       uniqs = "select distinct species as species from iris"),
#'    include_params = TRUE
#' )
#'
#' ## pass an env of interpolation values to the 'values' parameter
#' ## result of a single, unnamed query is returned as an object, not a
#' ## 1-element list
#' e <- new.env()
#' e$n <- 2
#' run_queries(
#'     "select * from iris limit {n}",
#'     values = e
#' )
#'
#' ## Use the execmethod parameter for statements
#' run_queries("create table iris_setosa as select * from iris where species = 'setosa'",
#'           execmethod = 'execute')
#'
#' run_queries("select distinct species as species from iris_setosa")
#'
#' @family SQL runners
#'
#' @seealso [read_sql()], [prepare_sql()]
#'
#' @export
run_queries <- function(sql,
                        ...,
                        default.conn = default_conn(),
                        include_params = FALSE) {
  if (is.null(default.conn) & is.null(connection_info())) {
    connect()
    default.conn = default_conn()
  }

  if (!(methods::is(default.conn, "DBIConnection") |
        methods::is(default.conn, "Pool"))) {
    if (is.null(connection_info()))
      stop("No connections are configured")

    if (!is.character(default.conn))
      stop("default.conn must be a connection or the name of a connection")

    if (default.conn %in% names(connection_cache))
      default.conn <- live_connection(conn_name = default.conn)

    if (!(methods::is(default.conn, "DBIConnection") |
          methods::is(default.conn, "Pool")))
      stop(glue::glue("default_conn is not a connection or the name of a connection"))

  }

  args <- list(...)
  args$sql <- sql
  args$default.conn <- default.conn
  if (!("values" %in% names(args))) {
    args$values <- parent.frame()
  }
  prepped_sql <- do.call(prepare_sql,
                         args)

  reqd_cols <-
    c("execmethod", "conn_name", "geometry", "prepared_sql")
  prepped_sql$result <- purrr::pmap(prepped_sql[, reqd_cols],

                                    function(execmethod,
                                             conn_name,
                                             geometry,
                                             prepared_sql,
                                             default.conn) {
                                      if (execmethod == "get")
                                        dispatcher <-
                                          DBI::dbGetQuery
                                      else if (execmethod == "execute")
                                        dispatcher <- DBI::dbExecute
                                      else if (execmethod == "sendq")
                                        dispatcher <-
                                          DBI::dbSendQuery
                                      else if (execmethod == "sends")
                                        dispatcher <-
                                          DBI::dbSendStatement
                                      else if (execmethod == "spatial")
                                        dispatcher <- sf::st_read
                                      else
                                        stop(
                                          glue::glue(
                                            "execmethod must be one of 'get', 'execute', 'sendq', 'sends' or 'spatial', not {execmethod}"
                                          )
                                        )

                                      if (conn_name == "default")
                                        conn <- default.conn
                                      else
                                        conn <-
                                          live_connection(conn_name)

                                      if (execmethod == "spatial" &&
                                          !is.na(geometry)) {
                                        result <- dispatcher(dsn = conn,
                                                             query = prepared_sql,
                                                             geometry_column = geometry)
                                      } else if (execmethod == "spatial") {
                                        result <- dispatcher(dsn = conn,
                                                             query = prepared_sql)
                                      } else {
                                        result <- dispatcher(conn = conn,
                                                             statement = prepared_sql)
                                      }
                                    }, # close function definition

                                    #default.conn = parent.frame()$default.conn
                                    default.conn = default.conn) # close purrr::pmap

  execd_sql <- tibble::as_tibble(prepped_sql)

  if (include_params) {
    out <- execd_sql
  } else {
    out <- execd_sql$result
    names(out) <- execd_sql$qname
  }

  if (length(out) == 1 && is.na(names(out)))
    out <- out[[1]]
  out
}

#' @rdname run_queries
#' @export
runqueries <- run_queries

#' Read, prepare and execute .SQL files
#'
#' Accepts a character vector of SQL file names and attempts to execute the
#' queries in each one.
#'
#' @param filenames name, or vector of names, of file(s) to be executed
#' @param ... Arguments to be passed to [run_queries()], [prepare_sql()], or [read_sql()]
#' @param include_params \code{TRUE} or \code{FALSE}. Should the parameters be
#'   included in the output?
#'
#' @return A list of results of sql queries found in files
#' @details If no default connection is supplied via `default.conn` and no
#'   connections have been configured using `connect()`, an attempt will be made
#'   to configure connections via `connect()` using the configuration search
#'   path. If no database connections are available after this attempt, an error
#'   will be raised. See `vignette("connections")` for details about the
#'   configuration search path.
#'
#' [run_files()] calls [read_sql()] on each file, and [prepare_sql()] on the
#' queries read from those files. Prepared queries are executed with [run_queries()]. The
#' behaviour of those functions can be controlled by passing the relevant
#' parameters to [run_files()] as the `...` argument.
#'
#' [run_files()] also enables control of the arguments accepted by [run_queries()] on
#' a per-query basis, by interpreting comments in SQL files as described
#' for `read_sql()`. Interpreted comments precede the sql query to which they
#' refer. Each interpretable comment must be on a line by itself and take the
#' form:
#'
#' ```sql
#' -- keyword = value
#' ```
#'
#' Keywords and possible values for interpretable comments are:
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
#' All interpreted comments except `qname` are cascaded _within their file_,
#' meaning that if you want to use the same values throughout, you need only set
#' them for the first query. See `read_sql()` for details.
#'
#' @examples
#' library(sqlhelper)
#'
#' config_filename <- system.file("examples/sqlhelper_db_conf.yml",
#'                 package="sqlhelper")
#'
#' readLines( config_filename ) |> writeLines()
#'
#' connect(
#'     config_filename,
#'     exclusive=TRUE)
#'
#' DBI::dbWriteTable( default_conn(), "iris", iris)
#'
#' sf::st_write(spData::congruent, default_conn(), "congruent")
#' sf::st_write(spData::incongruent, live_connection("pool_sqlite"), "incongruent")
#'
#' run_files_ex1 <- system.file("examples/run_files_ex1.sql", package="sqlhelper")
#' readLines( run_files_ex1 ) |> writeLines()
#'
#' run_files_ex2 <- system.file("examples/run_files_ex2.sql", package="sqlhelper")
#' readLines( run_files_ex2 ) |> writeLines()
#'
#' n_longest_petals <- 5
#' results <- run_files( c( run_files_ex1, run_files_ex2 ) )
#'
#' names(results)
#'
#' results$how_many_irises
#'
#' results$n_longest_setosa_petal_lengths
#'
#' plot(results$get_congruent, border = "orange")
#' plot(results$get_incongruent, border = "blue", add=TRUE)
#'
#' @family SQL runners
#' @seealso [read_sql()], [prepare_sql()]
#' @export
run_files <- function(filenames,
                      ...,
                      include_params = FALSE) {
  args <- list(...)
  if (!('cascade' %in% names(args)))
    args$cascade = TRUE

  sql <- do.call(rbind,
                 lapply(filenames, read_sql, cascade = args$cascade))

  args$cascade <-
    NULL # drop cascade to prevent 'unused argument' errors

  args$sql <- sql
  args$include_params <- include_params
  if (!("values" %in% names(args))) {
    args$values <- parent.frame()
  }
  do.call(runqueries,
          args)
}

#' @rdname run_files
#' @export
runfiles <- run_files
