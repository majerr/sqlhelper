#' Return the named connection or NULL
#'
#' @param conn_name Chr. The name of the live connection you want (use
#'   [connection_info] to get names of available connections).
#'
#' @return A live connection to a database, or NULL, invisibly, if
#'   `conn_name` is not the name of a live connection
#'
#' @examples
#' library(sqlhelper)
#' connect(
#'   system.file("examples/sqlhelper_db_conf.yml",
#'               package="sqlhelper")
#' )
#' connection_info()
#'
#' conn <- live_connection("simple_sqlite")
#' conn
#'
#' DBI::dbDisconnect(conn)
#' is.null(live_connection("simple_sqlite"))
#' is.null(live_connection("foo"))
#'
#' @export
live_connection <- function(conn_name) {
  tryCatch({
    if(is_connected(conn_name)){
      return(connection_cache[[conn_name]]$conn)
    } else {
      return(invisible(NULL))
    }
  },
  error = function(e){
    return(invisible(NULL))
  })
}
