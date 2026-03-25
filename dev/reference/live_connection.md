# Return the named connection or NULL

Return the named connection or NULL

## Usage

``` r
live_connection(conn_name)
```

## Arguments

- conn_name:

  Chr. The name of the live connection you want (use
  [connection_info](https://majerr.github.io/sqlhelper/dev/reference/connection_info.md)
  to get names of available connections).

## Value

A live connection to a database, or NULL, invisibly, if `conn_name` is
not the name of a live connection

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

conn <- live_connection("simple_sqlite")
conn
#> <SQLiteConnection>
#>   Path: 
#>   Extensions: TRUE

DBI::dbDisconnect(conn)
is.null(live_connection("simple_sqlite"))
#> [1] TRUE
is.null(live_connection("foo"))
#> [1] TRUE
```
