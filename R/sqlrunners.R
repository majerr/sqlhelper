#' Execute a sequence of SQL queries
#'
#' Accepts a character vector of SQL queries and runs each one
#' on either hive or postgresql.
#'
#' @param queries A list or character vector containing your sql commands
#' @param db Defaults to 'cds', which is the only option on the DAP
#' @param interpolate Should the SQL be parameterized from R? Defaults to the
#'   value of \code{parent.frame()}. May be set to \code{FALSE} if interpolation is
#'   to be avoided, or to another environment to control the source of the used.
#'   This can be useful if \code{sqlhelper} is used to run SQL from within another
#'   package, and you don't want to interfere with \code{.GlobalEnv}.
#' @return A list containing the results of each query
#' @family SQL runners
#' @seealso \code{\link{runfiles}}
#' @export
runqueries <- function(queries, db=default_conn_name, interpolate=parent.frame()){
  # If runqueries is called from runfiles with no parameter set and no db
  # spec'ed in the sql, the db parameter can be set to NA. If that happens, we
  # want it reset to the default before proceeding.
  if(is.na(db)){db <- default_conn_name}

  if(not.connected(db)){
    # This grep checks whether db is enclosed in quotes
    if( grepl("^[\"\'][^\"\']*[\"\']$",db) == TRUE){
      quotewarn <- " Try removing the quotes."
    }
    else {quotewarn <- ""}
    message("There were connection errors. Please check your output carefully.")
    return(
      list(
        error=stringr::str_interp("No connection to ${db}.${quotewarn}")
      )
    )
  }
  else{
    #runparams is an internal function. Definition is in R/connections.R
    dbparms <- getrunparams(db)
  }

  # parameterize queries before submitting, if required
  if(is.environment(interpolate) == TRUE){
    queries <- lapply(queries,glue::glue,.envir=interpolate)
  }

  # Submit with the appropriate connection and return the result
  results <- lapply(queries, dbparms$runner, conn=dbparms$conn)

  #Flatten the result, if there's only one
  if(length(results) == 1){
    results <- results[[1]]
  }
  return(results)
}

#' execute a \strong{single} file of SQL queries and obtain a flat list of results.
#'
#' This function is used internally and not exported, use \code{\link{runfiles}} instead.
#'
#' @param fn is a file name.
#' @param db is passed from runfiles and defaults to NA - the preferred method, for transparency,
#'           is to use an interpreted comment inside the sql file.
#' @param interpolate defaults to the value of \code{parent.frame()}. May be set to \code{FALSE} if
#'        interpolation to be avoided, or to another environment to control the source of the used. This
#'        can be useful if UC SQL Helper is used to run SQL from within another package, and you don't want
#'        to interfere with \code{.GlobalEnv}. The \code{\link[uccaseload]{UC Caseload package}} contains
#'        examples of this.
#' @return If the file contains more than one query, a list. Each element contains the results of
#'        each query.
#'
#'        If the file contains a single query, the result of that query is returned.
runfile <- function(fn,db=NA,interpolate=parent.frame()){
  sql <- read_sql_file(fn)

  # A dbname from the file takes priority over a parameter
  if(is.na(sql$db)==FALSE){
    db <- sql$db
  }
  else{
    db <- db
  }

  return(runqueries(sql$queries, db=db, interpolate=interpolate))
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
#'   single element character vector for a \code{CREATE TABLE} statment.} }
#'
#'   \code{runfiles()} will try to flatten the list intelligently so that you
#'   don't have to name lots of containers that contain only one element to
#'   inspect your results. But this may mean that you get a dataframe where you
#'   were expecting a list, e.g. if your file contains only one query.
#'
#' @family SQL runners
#' @export
runfiles <- function(filenames,db=NA,interpolate=parent.frame()){
  results <- lapply(filenames,runfile,db=db, interpolate=interpolate)
  names(results) <- gsub("\\.\\S+","",basename(filenames))
  if(length(results)==1){
    results <- results[[1]]
  }
  return(results)
}

#' Show to-be-executed SQL code
#'
#' Allows inspection of interpolated SQL before it is submitted to the database
#'
#' @param sql A character vector containing either sql file names or queries
#' @param is.queries Are these queries or filenames? defaults to FALSE (i.e. filenames)
#' @return \code{showsql} returns one of:
#' \describe{
#' \item{\strong{A list of lists} (for multiple files)}{The outer list contains one list for each file in filenames. Each inner list contains interpolated queries from that file.}
#' \item{\strong{A list} (for single files containing several queries, or vectors of queries)}{Each element is an interpolated query.}
#' \item{\strong{A single element character vector} (for single queries or files containing a single query).}{A single interpolated SQL query}
#' }
#'
#' @family SQL runners
#' @export
#'
showsql <- function(sql,is.queries=FALSE,interpolate=parent.frame()){
  if(is.queries == FALSE){
    filenames <- sql
    sql <- lapply(sql,read_sql_file)
    sql <- lapply(sql, function(sql){sql$queries})
    names(sql) <- gsub("\\.\\S+","",basename(filenames))
    if(is.environment(interpolate)==TRUE){
      sql <- lapply(sql,function(x){
        lapply(x,glue::glue,.envir=interpolate)
      })
    }
  }
  else{
    if(is.environment(interpolate)==TRUE){
      sql <- lapply(sql,glue::glue)
    }
  }

  if(length(sql)==1){
    return(sql[[1]])
  }
  else{
    return(sql)
  }
}
