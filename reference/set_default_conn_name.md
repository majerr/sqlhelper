# Set/get the name of the default connection to use

Set/get the name of the default connection to use

## Usage

``` r
set_default_conn_name(conn_name)

get_default_conn_name()
```

## Arguments

- conn_name:

  Character string. The name a connection

## Value

`get` returns the name of the default connection; `set` returns `NULL`,
invisibly.

## Examples

``` r
library(sqlhelper)
connect(
    system.file("examples/sqlhelper_db_conf.yml",
                package="sqlhelper"),
    exclusive = TRUE
)

connection_info()
#> # A tibble: 2 × 7
#>   name          description                  live  default driver conn_str pool 
#>   <chr>         <chr>                        <lgl> <lgl>   <glue> <chr>    <lgl>
#> 1 pool_sqlite   A pooled connection to an i… TRUE  FALSE   RSQLi… NA       TRUE 
#> 2 simple_sqlite A simple connection to an i… TRUE  TRUE    RSQLi… NA       FALSE

get_default_conn_name()
#> [1] "simple_sqlite"

set_default_conn_name("pool_sqlite")

connection_info()
#> # A tibble: 2 × 7
#>   name          description                  live  default driver conn_str pool 
#>   <chr>         <chr>                        <lgl> <lgl>   <glue> <chr>    <lgl>
#> 1 pool_sqlite   A pooled connection to an i… TRUE  TRUE    RSQLi… NA       TRUE 
#> 2 simple_sqlite A simple connection to an i… TRUE  FALSE   RSQLi… NA       FALSE

get_default_conn_name()
#> [1] "pool_sqlite"
```
