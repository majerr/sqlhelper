#' Parameterize SQL queries
#'
#' Accepts a list or character vector of SQL queries and parameterizes each.
#'
#' @param sql A list or character vector of sql queries or file names. If any
#'   elements do not name an existing file, all are treated as sql queries.
#'
#' @param interpolate An environment containing values for interpolation, or
#'   \code{FALSE.} If \code{FALSE}, no interpolation is done. Defaults to
#'   \code{parent.frame()}
#'
#' @param quote The name of a connection to be used by \code{glue::glue_sql} or
#'   \code{FALSE}. If \code{FALSE}, \code{glue::glue} is used. Defaults to
#'   \code{default_conn_name}.
#'
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
#' tbl_sql <- interpolate_sql("select {var} from {`table_name`} limit {n}")
interpolate_sql <- function(sql,
                            interpolate=parent.frame(),
                            quote = default_conn_name){
  if(!is.character(sql)){
    stop(glue::glue("Expecting sql character vector, got {typeof(sql)}."))
  }

  is.files <- unlist(lapply(sql,file.exists))
  is.files <- (sum(is.files) == length(is.files))

  do.interpolate <- is.environment(interpolate)
  do.quote <- c(quote) %in% connections

  if(is.files){
    filenames <- sql
    sql <- lapply(
      lapply(sql,read_sql_file),
      function(sql){sql$queries}
    )
    names(sql) <- gsub("\\.\\S+","",basename(filenames))
  }

    if(do.interpolate){
    sql <- dplyr::case_when(
      # Interpolate and quote SQL from files
      is.files & do.quote ~ lapply(
        sql,
        function(x){
          lapply(x, glue::glue_sql, .envir=interpolate, .con=quote)
        }
      ),

      # Just interpolate from files, no quoting
      is.files ~ lapply(
        sql,
        function(x){
          lapply(x, glue::glue, .envir=interpolate,)
        }
      ),

      # Interpolate and quote SQL queries (not from files)
      do.quote ~ lapply(sql,glue::glue_sql, .envir=interpolate, .con=quote),

      # Just interpolate queries (not from files), no quoting
      TRUE ~ lapply(sql,glue::glue, .envir=interpolate)
    )
  }

  sql
}

