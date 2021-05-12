# This file contains several (mainly) internal functions
# used either by exported functions (e.g. those in defined in sqlrunners.R)
# or called by .onLoad()

# set_connections() is called by .onLoad()
#
# Connections is a predefined list, loaded from R/sysdata.rda, with two
# elements:
#
# > names(connections)
# [1] "hive" "pg"
#
# set_connections() sets hive to an open RODBC connection and
# pg to an open RPostgresSQL connection.
#
# To add more in the future, first add the name of the new connection in R/sysdata.rda
# and then specify the connection in this function
set_connections <- function(){

  # CDS SQLServer
  tryCatch({
    suppressWarnings({
      connections$cds <<- DBI::dbConnect(odbc::odbc(),
                                         .connection_string="
                                          Driver={ODBC Driver 17 for SQL Server};
                                          Server=Dap-sql01\\cds;
                                          Trusted_Connection=yes")
    })
  },
  error = function(c) {
    warning("CDS is not available")
  })

  # Add more connections here. Use the <<- assignment to access the connections list.
}

#

# Returns a connection and a sql runnner for the db parameter.
# For internal use only!
getrunparams <- function(db){
  db <- tolower(db)
  if(db == "cds"){
    return(
      list(
        conn = connections$cds,
        runner = DBI::dbGetQuery,
        is.live = DBI::dbIsValid)
    )
  }
  # Add more else if clauses here if more connections are required
  else{
    return(list(conn=NA,
                runner=NA))
  }
}

#' Test whether a database is connected
#'
#' @param db Which database do you want to test? (restricted to 'cds' in MHCLG)
#'
#' @return Boolean
#'
#' is.connected and not.connected provide generic, semantically transparent
#' methods of checking whether a connection is live, irrespective of whether it
#' is through RODBC, RPostgreSQL, DBI etc. They depend on the is.live functions
#' provided by getrunparams (see above).

#' is.connected returns FALSE if the db connection returned by getrunparams is
#' not valid.
#' @export
is.connected <- function(db){
  runparms <- getrunparams(db)
  out <- NA
  tryCatch({out <- runparms$is.live(runparms$conn)},
           error = function(e){
             out<<-TRUE
           })
  return(out)
}

#' Test whether a database is connected
#'
#' @param db Which database do you want to test? (restricted to 'cds' in MHCLG)
#'
#' @return Boolean
#'
#' is.connected and not.connected provide generic, semantically transparent
#' methods of checking whether a connection is live, irrespective of whether it
#' is through RODBC, RPostgreSQL, DBI etc. They depend on the is.live functions
#' provided by getrunparams (see above).
#'
#' not.connected returns TRUE if the db connection returned by getrunparams is
#' not valid
#' @export
not.connected <- function(db){
  runparms <- getrunparams(db)
  out <- NA
  tryCatch({out <- runparms$is.live(runparms$conn)},
           error = function(e){
             out<<-FALSE
           })
  return(!out)
}

#' Close connections to Hive and PostgreSQL
#'
#' This function is run when the library is unloaded
close_connections <- function(){
  if(is.connected('cds')){
    suppressWarnings(DBI::dbDisconnect(connections$cds))
  }
  connections$cds <- NA
}

#' Re-establish connections to all configured databases
#'
#' Closes and then re-opens connections all configured connections
#'
#' @export
reconnect <- function(){
  close_connections()
  set_connections()
}

