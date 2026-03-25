# prepare queries and assemble meta data prior to execution

Except for `sql`, parameters are default values to be used when none are
supplied in `sql` (i.e. when `sql` is a tibble returned by
[`read_sql()`](https://majerr.github.io/sqlhelper/reference/read_sql.md)).

## Usage

``` r
prepare_sql(
  sql,
  quotesql = "yes",
  values = parent.frame(),
  execmethod = "get",
  geometry = NA,
  default.conn = default_conn()
)
```

## Arguments

- sql:

  An optionally-named list or character vector containing sql commands,
  or a tibble returned by
  [`read_sql()`](https://majerr.github.io/sqlhelper/reference/read_sql.md)

- quotesql:

  "yes" or "no" - should interpolated characters be quoted by default?
  Anything that isn't "no" is treated as "yes".

- values:

  An environment containing variables to interpolate into the SQL. Pass
  any object that is not an environment (commonly-used options include
  "no", NA, FALSE or NULL) if interpolation is to be skipped, or another
  environment containing values to interpolate to avoid using
  `.GlobalEnv`.

- execmethod:

  One of "get", "execute", "sendq", "sends" or "spatial" - which method
  should be used to execute the query? "get" means
  [`DBI::dbGetQuery()`](https://dbi.r-dbi.org/reference/dbGetQuery.html);
  "execute" means
  [`DBI::dbExecute()`](https://dbi.r-dbi.org/reference/dbExecute.html);
  "sendq" means
  [DBI::dbSendQuery](https://dbi.r-dbi.org/reference/dbSendQuery.html);
  "sends" means
  [`DBI::dbSendStatement()`](https://dbi.r-dbi.org/reference/dbSendStatement.html);
  "spatial" means
  [`sf::st_read()`](https://r-spatial.github.io/sf/reference/st_read.html).

- geometry:

  If `execmethod` is "spatial", which column contains the geometry?
  Ignored if `execmethod` is not "spatial".

- default.conn:

  Either the name of a sqlhelper connection, or a database connection
  returned by
  [`DBI::dbConnect()`](https://dbi.r-dbi.org/reference/dbConnect.html)
  or
  [`pool::pool()`](http://rstudio.github.io/pool/reference/pool-package.md),
  or NA. This connection is only used by
  [`glue::glue_sql()`](https://glue.tidyverse.org/reference/glue_sql.html)
  to quote SQL interpolations; `prepare_sql()` does not execute any SQL
  code.

## Value

A tibble containing 1 row per query with the following fields:

- qname:

  character. A name for this query

- quotesql:

  "yes" or "no". Should parameterized character values be quoted for
  this query?

- interpolate:

  "yes" or "no". Should this query be parameterized with values from R?

- execmethod:

  The method to execute this query. One of "get"
  ([`DBI::dbGetQuery()`](https://dbi.r-dbi.org/reference/dbGetQuery.html)),
  "execute"
  ([`DBI::dbExecute()`](https://dbi.r-dbi.org/reference/dbExecute.html)),
  "sendq"
  ([`DBI::dbSendQuery()`](https://dbi.r-dbi.org/reference/dbSendQuery.html)),
  "sends"
  ([`DBI::dbSendStatement()`](https://dbi.r-dbi.org/reference/dbSendStatement.html))
  or "spatial"
  ([`sf::st_read()`](https://r-spatial.github.io/sf/reference/st_read.html))

- geometry:

  character. If `execmethod` is "spatial", which is the geometry column?

- conn_name:

  character. The name of the database connection to use for this query.
  Must be the name of a configured sqlhelper connection.

- sql:

  The sql query as entered

- filename:

  The value of `file_name`

- prepared_sql:

  The sql query to be executed, i.e. with interpolations and quoting in
  place

## Details

The `default.conn` parameter may be used to supply a connection object
that is not a configured sqlhelper connection which can then be used to
interpolate quoted strings.

## Examples

``` r
library(sqlhelper)
connect(
    system.file("examples/sqlhelper_db_conf.yml",
                package="sqlhelper"),
    exclusive = TRUE
)

n <- 5
foo <- 'bar'
prepped <- prepare_sql(c("select {`foo`}", "select {n}"))
prepped
#> # A tibble: 2 × 9
#>   qname quotesql interpolate execmethod geometry conn_name sql          filename
#>   <chr> <chr>    <chr>       <chr>      <chr>    <chr>     <chr>        <chr>   
#> 1 NA    yes      yes         get        NA       default   select {`fo… NA      
#> 2 NA    yes      yes         get        NA       default   select {n}   NA      
#> # ℹ 1 more variable: prepared_sql <list>
prepped$prepared_sql
#> [[1]]
#> <SQL> select `bar`
#> 
#> [[2]]
#> <SQL> select 5
#> 
```
