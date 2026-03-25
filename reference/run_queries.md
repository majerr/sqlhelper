# Execute a sequence of SQL queries

Accepts a character vector of SQL queries and attempts to execute each

## Usage

``` r
run_queries(sql, ..., default.conn = default_conn(), include_params = FALSE)

runqueries(sql, ..., default.conn = default_conn(), include_params = FALSE)
```

## Arguments

- sql:

  An optionally-named list or character vector containing sql strings,
  or a tibble returned by
  [`read_sql()`](https://majerr.github.io/sqlhelper/reference/read_sql.md)
  or
  [`prepare_sql()`](https://majerr.github.io/sqlhelper/reference/prepare_sql.md).

- ...:

  Arguments to be passed to
  [`read_sql()`](https://majerr.github.io/sqlhelper/reference/read_sql.md)
  or
  [`prepare_sql()`](https://majerr.github.io/sqlhelper/reference/prepare_sql.md)

- default.conn:

  Either the name of a sqlhelper connection, or a database connection
  returned by
  [`DBI::dbConnect()`](https://dbi.r-dbi.org/reference/dbConnect.html)
  or
  [`pool::dbPool()`](http://rstudio.github.io/pool/reference/dbPool.md).
  This connection is used as a fall-back when the `sql` parameter is a
  tibble and no per-query connection name is supplied, or the connection
  name is `default` (see
  [`prepare_sql()`](https://majerr.github.io/sqlhelper/reference/prepare_sql.md)).
  It may be used by
  [`glue::glue_sql()`](https://glue.tidyverse.org/reference/glue_sql.html)
  to interpolate SQL strings, and as the connection against which to
  execute SQL queries.

- include_params:

  `TRUE` or `FALSE`. Should the parameters be included in the output?
  Mainly useful for debugging.

## Value

- If `include_params` is `FALSE` and the `sql` argument is a vector, a
  list containing the results of each query; element names will be taken
  from the `sql` argument.

- If the length of the `sql` argument is 1 and is not named, the result
  of that query is returned as-is (e.g. a data.frame), not as a
  1-element list.

- If `include_params` is `TRUE`, a tibble is returned containing 1 row
  per query with the following fields:

&nbsp;

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

  character. If `execmethod` is "spatial", this should be the name of
  the geometry column.

- conn_name:

  character. The name of the database connection against which to
  execute this query. Must be the name of a configured sqlhelper
  connection.

- sql:

  The sql query to be executed

- filename:

  The value of `file_name`

- prepared_sql:

  The sql query to be executed, i.e. with interpolations and quoting in
  place

- result:

  The result of the query

## Details

If no default connection is supplied via `default.conn` and no
connections have been configured using
[`connect()`](https://majerr.github.io/sqlhelper/reference/connect.md),
an attempt will be made to configure connections via
[`connect()`](https://majerr.github.io/sqlhelper/reference/connect.md)
using the configuration search path. If no database connections are
available after this attempt, an error will be raised. See
[`vignette("connections")`](https://majerr.github.io/sqlhelper/articles/connections.md)
for details about the configuration search path.

## See also

[`read_sql()`](https://majerr.github.io/sqlhelper/reference/read_sql.md),
[`prepare_sql()`](https://majerr.github.io/sqlhelper/reference/prepare_sql.md)

Other SQL runners:
[`run_files()`](https://majerr.github.io/sqlhelper/reference/run_files.md)

## Examples

``` r
library(sqlhelper)

readLines(
    system.file("examples/sqlhelper_db_conf.yml",
                package="sqlhelper")
    ) |>
writeLines()
#> #### sqlhelper_db_conf.yml ####
#> 
#> simple_sqlite:
#>   driver_type: sqlite
#>   description: "A simple connection to an in-memory database"
#>   connection:
#>     Server: ":memory:"
#> 
#> pool_sqlite:
#>   driver_type: sqlite
#>   pool: yes
#>   description: "A pooled connection to an in-memory database"
#>   connection:
#>     Server: ":memory:"

connect(
    system.file("examples/sqlhelper_db_conf.yml", package="sqlhelper"),
    exclusive=TRUE)

DBI::dbWriteTable( default_conn(),
                  "iris",
                  iris)

n <- 5

run_queries(
    c(top_n = "select * from iris limit {n}",
      uniqs = "select distinct species as species from iris")
)
#> $top_n
#>   Sepal.Length Sepal.Width Petal.Length Petal.Width Species
#> 1          5.1         3.5          1.4         0.2  setosa
#> 2          4.9         3.0          1.4         0.2  setosa
#> 3          4.7         3.2          1.3         0.2  setosa
#> 4          4.6         3.1          1.5         0.2  setosa
#> 5          5.0         3.6          1.4         0.2  setosa
#> 
#> $uniqs
#>      species
#> 1     setosa
#> 2 versicolor
#> 3  virginica
#> 

## use include_params to review the execution context
run_queries(
    c(top_n = "select * from iris limit {n}",
      uniqs = "select distinct species as species from iris"),
   include_params = TRUE
)
#> # A tibble: 2 × 10
#>   qname quotesql interpolate execmethod geometry conn_name sql          filename
#>   <chr> <chr>    <chr>       <chr>      <chr>    <chr>     <chr>        <chr>   
#> 1 top_n yes      yes         get        NA       default   select * fr… NA      
#> 2 uniqs yes      yes         get        NA       default   select dist… NA      
#> # ℹ 2 more variables: prepared_sql <list>, result <list>

## pass an env of interpolation values to the 'values' parameter
## result of a single, unnamed query is returned as an object, not a
## 1-element list
e <- new.env()
e$n <- 2
run_queries(
    "select * from iris limit {n}",
    values = e
)
#>   Sepal.Length Sepal.Width Petal.Length Petal.Width Species
#> 1          5.1         3.5          1.4         0.2  setosa
#> 2          4.9         3.0          1.4         0.2  setosa

## Use the execmethod parameter for statements
run_queries("create table iris_setosa as select * from iris where species = 'setosa'",
          execmethod = 'execute')
#> [1] 0

run_queries("select distinct species as species from iris_setosa")
#>   species
#> 1  setosa
```
