# This file contains several 'under-the-hood' functions (i.e. not exported)
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

  # Hive
  tryCatch({
    # Prefer Hive2 (i.e. Hadoop 3.1)
    suppressWarnings({
      connections$hive <<- RODBC::odbcConnect("Hive2")
      RODBC::sqlQuery(connections$hive,"set role all")
      connections$hive_version <<- "Hive 3.1"
    })
  },
  error = function(e){
    connections$hive_error <<- e
    # If Hive2 isn't available, try Hive (Hadoop 2.6.5)
    tryCatch({
      suppressWarnings({
        connections$hive <<- RODBC::odbcConnect("Hive")
        RODBC::sqlQuery(connections$hive,"set role all")
        connections$hive_version <<- "Hive 2.6.5"
      })
    },
    error = function(e){
      connections$hive_error <<- e
      warning("Hive database is not available")
    })
  })

  # PostgreSQL
  pfn <- "~/.postgres-pass.txt"
  if(!file.exists(pfn)){
    pw <- getPass::getPass(msg="Please enter your PostgreSQL password (this time only)")
    f <- file(pfn)
    writeLines(pw,f)
    close(f)
  }
  tryCatch({
    suppressWarnings({
      connections$pg <<-
        RPostgreSQL::dbConnect(
        DBI::dbDriver("PostgreSQL"),
        dbname = "uc",
        host = "pgetl01.prd.tac.dw",
        port = 5432,
        user = Sys.info()[["user"]], # Assumes postgresql username is the same as RStudio unix username (it should be)
        password = trimws(
          readChar(
            pfn, # Assumes postgresql password is stored in "~/.postgres-pass.txt"
            file.info(pfn)$size)
          )
        )
    })
  },
  error = function(c) {
    warning("PostgreSQL database is not available")
    }
  )

  # Add more connections here. Use the <<- assignment to access the connections list.
}

#

# Returns a connection and a sql runnner for the db parameter.
# For internal use only!
getrunparams <- function(db){
  db <- tolower(db)
  if(db == "hive" ||
     db == "h"){
    return(
      list(
        conn = connections$hive,
        runner = RODBC::sqlQuery,
        is.live = RODBC:::odbcValidChannel)
    )
  }
  else if(db == "postgres" ||
          db == "postgresql" ||
          db == "pg" ||
          db == "p"){
    return(
      list(
        conn = connections$pg,
        runner = RPostgreSQL::dbGetQuery,
        is.live = RPostgreSQL::isPostgresqlIdCurrent)
    )
  }
  # Add more else if clauses here if more connections are required
  else{
    return(list(conn=NA,
                runner=NA))
  }
}

# is.connected and not.connected provide generic, semantically transparent
# methods of checking whether a connection is live, irrespective of whether it
# is through RODBC, RPostgreSQL, DBI etc. They depend on the is.live functions
# provided by getrunparams (see above).

# is.connected returns FALSE if the db connection returned by getrunparams is
# not valid.
is.connected <- function(db){
  runparms <- getrunparams(db)
  out <- NA
  tryCatch({out <- runparms$is.live(runparms$conn)},
           error = function(e){
             out<<-TRUE
           })
  return(out)
}

# not.connected returns TRUE if the db connection returned by getrunparams is
# not valid
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
  if(is.connected('hive')){
    suppressWarnings(RODBC::odbcClose(connections$hive))
  }
  connections$hive <- NA

  if(is.connected('postgres')){
    suppressWarnings(RPostgreSQL::dbDisconnect(connections$pg))
  }
  connections$pg <- NA
}

#' Re-establish connections to Hive and PostgreSQL
#'
#' Closes and then re-opens connections to Hive and PostgreSQL
#'
#' @export
reconnect <- function(){
  close_connections()
  set_connections()
}

#' Print the version of Hive that the hive connection points to.
#' @export
hiver <- function(){
  print(connections$hive_version)
}

#' Print the most recent connection error from Hive
#' @export
hiverr <- function(){
  print(connections$hive_error)
}
