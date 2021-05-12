# sqlgenerators.R
#
# Functions that generate useful sql snippets.
# Functions can be used as parameters in sql code. Results will be interpolated before submission.

#' Restrict a query to a sample of 1/nth of the unique values of a key
#'
#' @param n nth of k
#' @param k key on which to restrict sample
#' @return string (sql code)
#' @family SQL Generators
#' @export
nthofk <- function(n,k){
  return(
    glue::glue("abs(binary_checksum({k})) % {n} < 1")
  )
}

#' Restrict a query to values found in a 1/nth sample of the unique values of a foreign key
#'
#' @param v variable to restrict
#' @param n nth of fk
#' @param f foreign table
#' @param k foreign key
#' @return string (sql code)
#' @family SQL Generators
#' @export
vinnthoffk <- function(v,n,f,k){
  return(
    glue::glue("{v} in (select {k} from {f} where abs(hash({k})) % {n} < 1)")
  )
}
