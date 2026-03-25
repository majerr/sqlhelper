# Test whether a database is connected

Test whether a database is connected

## Usage

``` r
is_connected(conn_name)

not_connected(conn_name)
```

## Arguments

- conn_name:

  Character. The name of a connection (run
  [`connection_info()`](https://majerr.github.io/sqlhelper/reference/connection_info.md)
  for options)

## Value

Logical, or NULL if `conn_name` does not identify exactly 1 connection

## Examples

``` r
library(sqlhelper)

connect(
  system.file("examples/sqlhelper_db_conf.yml",
              package="sqlhelper")
)
connection_info()
#> # A tibble: 2 × 7
#>   name          description                  live  default driver conn_str pool 
#>   <chr>         <chr>                        <lgl> <lgl>   <glue> <chr>    <lgl>
#> 1 pool_sqlite   A pooled connection to an i… TRUE  FALSE   RSQLi… NA       TRUE 
#> 2 simple_sqlite A simple connection to an i… TRUE  TRUE    RSQLi… NA       FALSE

is_connected("simple_sqlite")
#> [1] TRUE
is_connected("foo")
#> NULL
DBI::dbDisconnect(live_connection("simple_sqlite"))
is_connected("simple_sqlite")
#> [1] FALSE
not_connected("simple_sqlite")
#> [1] TRUE
disconnect()
is_connected("simple_sqlite")
#> NULL
not_connected("simple_sqlite")
#> NULL
```
