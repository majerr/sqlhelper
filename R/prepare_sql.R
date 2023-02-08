#' prepare to queries and meta data for running
#'
#' @param sql An optionally-named list or character vector containing sql
#'   commands, or a tibble returned by [read_sql()]
#' @param quotesql "yes" or "no" - should interpolated characters be quoted by
#'   default?
#' @param values Should the SQL be parameterized from R? Defaults to the
#'   value of `parent.frame()`. Pass any object that is not an environment
#'   (e.g. "no" or FALSE) if interpolation is to be skipped, or another
#'   environment containing values to interpolate to avoid using
#'   `.GlobalEnv`.
#' @param execmethod One of "get", "send" or "spatial" - which method should be
#'   used to execute the query? "get" means [DBI::dbGetQuery()]; "send" means
#'   [DBI::dbSendQuery()]; "spatial" means [sf::st_read()].
#' @param geometry If `execmethod` is "spatial", which column contains the
#'   geometry? (ignored if `execmethod` is not spatial)
#' @param default_conn Either the name of a sqlhelper connection, or a database
#'   connection returned by [DBI::dbConnect()], or NA
#' @export
prepare_sql <- function(sql,
                        quotesql = "yes",
                        values = parent.frame(),
                        execmethod = "get",
                        geometry = NA,
                        default_conn=NA){

  sql_names <- names(sql)
  sql <- tibble::as_tibble(sql)

  # sql was a list or character vector
  if( ncol(sql) == 1 && names(sql) == "value" ){

    sql <- dplyr::rename(sql, sql = value)

    if( ! is.null(sql_names) )
      sql$qname <- sql_names
    else
      sql$qname <- NA

    # TODO: this is inelegant; should use sql_tbl_names to add the remaining cols
    sql <- dplyr::mutate( sql,
                          quotesql = NA,
                          interpolate = NA,
                          execmethod = NA,
                          geometry = NA,
                          conn_name = NA,
                          filename = NA
    )

    # sql_tbl_names is defined in data-raw/sysdata.r
    sql <- dplyr::relocate(sql,
                           sql_tbl_names)
  }

  if( sum(names(sql) %in% sql_tbl_names) != length(sql_tbl_names) )
    stop("sql table does not contain the required columns")

  # Fill NA's with default values

  sql$quotesql[ is.na( sql$quotesql ) ] <- quotesql

  sql$execmethod[ is.na( sql$execmethod ) ] <- execmethod

  sql$geometry[ is.na( sql$geometry ) ] <- geometry

  sql$conn_name[ is.na( sql$conn_name ) ] <- "default"

  if( is.environment( values ) ){
    sql$interpolate[ is.na( sql$interpolate ) ] <- "yes"

    sql$prepared_sql <- purrr::pmap(
      dplyr::select( sql,
                     interpolate,
                     quotesql,
                     conn_name,
                     sql
      ),
      interpolate_sql,
      values = values,
      default_conn = default_conn
    )
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
                            values = parent.frame(),
                            default_conn = NULL
                            ){
  if( interpolate == "no" )
    return( sql )


  if( quotesql == "no" )

    interpolated <- glue::glue( sql,
                                .envir = values )

  else {
    if( conn_name == "default")
      live_conn <- default_conn
    else
      live_conn <- live_connection( conn_name )

    interpolated <- glue::glue_sql( sql,
                                    .envir = values,
                                    .con = live_conn)
  }

  interpolated
}


