#' prepare queries and assemble meta data prior to execution
#'
#' Except for `sql`, parameters are default values to be used when none are
#' supplied in `sql` (i.e. when `sql` is a tibble returned by [read_sql()]).
#'
#' The `default.conn` parameter may be used to supply a connection object that
#' is not a configured sqlhelper connection which can then be used to
#' interpolate quoted strings.
#'
#' @param sql An optionally-named list or character vector containing sql
#'   commands, or a tibble returned by [read_sql()]
#'
#' @param quotesql "yes" or "no" - should interpolated characters be quoted by
#'   default? Anything that isn't "no" is treated as "yes".
#'
#' @param values An environment containing variables to interpolate into the
#'   SQL. Pass any object that is not an environment (commonly-used options
#'   include "no", NA, FALSE or NULL) if interpolation is to be skipped, or
#'   another environment containing values to interpolate to avoid using
#'   `.GlobalEnv`.
#'
#' @param execmethod One of "get", "execute", "sendq", "sends" or "spatial" -
#'   which method should be used to execute the query? "get" means
#'   [DBI::dbGetQuery()]; "execute" means [DBI::dbExecute()]; "sendq" means
#'   [DBI::dbSendQuery]; "sends" means [DBI::dbSendStatement()]; "spatial" means
#'   [sf::st_read()].
#'
#' @param geometry If `execmethod` is "spatial", which column contains the
#'   geometry? Ignored if `execmethod` is not "spatial".
#'
#' @param default.conn Either the name of a sqlhelper connection, or a database
#'   connection returned by [DBI::dbConnect()] or [pool::pool()], or NA. This
#'   connection is only used by [glue::glue_sql()] to quote SQL interpolations;
#'   [prepare_sql()] does not execute any SQL code.
#'
#' @return A tibble containing 1 row per query with the following fields:
#'
#'   \describe{
#'   \item{qname}{character. A name for this query}
#'   \item{quotesql}{"yes" or "no". Should parameterized character values be
#'   quoted for this query?}
#'   \item{interpolate}{"yes" or "no". Should this query
#'   be parameterized with values from R?}
#'   \item{execmethod}{The method to
#'   execute this query. One of "get" ([DBI::dbGetQuery()]), "execute"
#'   ([DBI::dbExecute()]), "sendq" ([DBI::dbSendQuery()]), "sends"
#'   ([DBI::dbSendStatement()]) or "spatial" ([sf::st_read()])}
#'   \item{geometry}{character. If `execmethod` is "spatial", which is the
#'   geometry column?}
#'   \item{conn_name}{character. The name of the database
#'   connection to use for this query. Must be the name of a configured
#'   sqlhelper connection.}
#'   \item{sql}{The sql query as entered}
#'   \item{filename}{The value of `file_name`}
#'   \item{prepared_sql}{The sql query
#'   to be executed, i.e. with interpolations and quoting in place}
#'   }
#'
#' @examples
#' library(sqlhelper)
#' connect(
#'     system.file("examples/sqlhelper_db_conf.yml",
#'                 package="sqlhelper")
#' )
#'
#' n <- 5
#' foo <- 'bar'
#' prepped <- prepare_sql(c("select {`foo`}", "select {n}"))
#' prepped
#' prepped$prepared_sql
#'
#' @export
prepare_sql <- function(sql,
                        quotesql = "yes",
                        values = parent.frame(),
                        execmethod = "get",
                        geometry = NA,
                        default.conn = default_conn()
                        ){

  ## INPUT VALIDATION
  sql_is_tibble <- tibble::is_tibble(sql)
  if(sql_is_tibble){
    correct_colnames <- (sum(names(sql) %in% sql_tbl_names) == length(sql_tbl_names))
  }

  if(!sql_is_tibble && !is.character(sql))
      stop(
        glue::glue("sql argument must be a character vector or a tibble, not a {typeof(sql)}")
      )

  if(sql_is_tibble && !correct_colnames){
    stop(
      glue::glue("sql tibbles must have these columns: {paste(sql_tbl_names, collapse=', ')};
                  not {paste(names(sql), collapse=', ')}")
    )
  }

  if(sql_is_tibble && sum(is.na(sql$sql)) > 0)
    stop("sql argument contains NAs in the sql column")

  sql_names <- names(sql)

  if(!sql_is_tibble){
    sql <- tibble::as_tibble(sql)
    colnames(sql) <- "sql"

    if( ! is.null(sql_names) )
      sql$qname <- sql_names
    else
      sql$qname <- NA

    # Add other needed columns and reorder
    # sql_tbl_names is defined in data-raw/sysdata.r
    new_col_names <- sql_tbl_names[ !sql_tbl_names %in% names(sql)]
    new_cols <- rep(NA, length(new_col_names))
    names(new_cols) <- new_col_names
    sql <- tibble::add_column( sql, !!!new_cols )[,sql_tbl_names]
  }

  # Fill NA's with default values

  sql$quotesql[ is.na( sql$quotesql ) ] <- quotesql

  sql$execmethod[ is.na( sql$execmethod ) ] <- execmethod

  sql$geometry[ is.na( sql$geometry ) ] <- geometry

  sql$conn_name[ is.na( sql$conn_name ) ] <- "default"

  # Convert any all-null columns to characters
  sql <- tibble::as_tibble(lapply(sql, as.character))

  if( is.environment( values ) ){
    sql$interpolate[ is.na( sql$interpolate ) ] <- "yes"

    #default.conn is a string - reassign as a connection
    if(methods::is(default.conn, "character")){
      default.conn <- live_connection(default.conn)
    }

    #check whether default.conn is a connection - error if not
    tryCatch({
      stopifnot(DBI::dbIsValid(default.conn))
      },
      error = function(e){
        stop("default.conn is not a valid connection")
      }
    )

    sql$prepared_sql <- sql[, c("interpolate", "quotesql", "conn_name", "sql")] |>
      purrr::pmap( interpolate_sql,
                   values = values,
                   default.conn = default.conn)

  } else {
    sql$interpolate <- "no"

    sql$prepared_sql <- sql$sql
  }

  sql
}

#' Parameterize SQL queries
#'
#' Accepts a list or character vector of SQL queries and parameterizes each.
#'
#' @param sql A tibble, as returned by [read_sql_file()]
#'
#' @param env An environment containing values for interpolation. Defaults to
#'   the value of [parent.frame()]
#'
#' @param default_conn A connection inheriting from DBI::DBIConnection. Used
#'   when the `conn` variable of the `sql` argument is set to
#'   'default'.
#'
#' @return A list or character vector of parameterized sql queries
#'
#' @noRd
interpolate_sql <- function(interpolate,
                            quotesql,
                            conn_name,
                            sql,
                            values,
                            default.conn
){

  if( interpolate == "no" )
    return( DBI::SQL(sql) )


  if( quotesql == "no" )
    tryCatch({
      interpolated <- DBI::SQL(
        glue::glue( sql,
                    .envir = values )
      )
    },
    error = function(e){
      stop(glue::glue("Could not interpolate '{sql}'. glue::glue error was:
                        {e}"))
    })

  else {
    if( conn_name == "default")
      live_conn <- default.conn
    else
      live_conn <- live_connection( conn_name )

    if(is.null(live_conn))
      stop(glue::glue("{conn_name} is not the name of a valid connection"))

    tryCatch({
      interpolated <- glue::glue_sql( sql,
                                      .envir = values,
                                      .con = live_conn)
    },
    error = function(e){
      stop(glue::glue("Could not interpolate '{sql}'. glue::glue_sql error was:
                        {e}"))
    })
  }

  interpolated
}


