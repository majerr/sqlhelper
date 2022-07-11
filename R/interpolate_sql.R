#' Parameterize SQL queries
#'
#' Accepts a list or character vector of SQL queries and parameterizes each.
#'
#' @param sql A list or character vector of sql queries or file names.
#'
#' @param interpolate An environment containing values for interpolation, or
#'   \code{FALSE.} If \code{FALSE}, no interpolation is done. Defaults to
#'   \code{parent.frame()}
#'
#' @param quote The name of a connection to be used by \code{glue::glue_sql} or
#'   \code{FALSE}. If \code{FALSE}, \code{glue::glue} is used. Defaults to
#'   \code{default_conn_name}.
#'
#' @details
#' @return A list or character vector of parameterized sql queries
#' @export
#' @examples
#' con <- DBI::dbConnect(RSQLite::SQLite(),":memory:")
#' iris2 <- iris
#' colnames(iris2) <- gsub("[.]", "_", tolower(colnames(iris)))
#' DBI::dbWriteTable(con, "iris", iris2)
#'
#' table_name <- "iris"
#' var <- "Sepal_Length"
#' n <- 10
#'
#' tbl_sql <- interpolate_sql("select * from {`table_name`}")
