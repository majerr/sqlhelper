# Return the default connection

A convenience wrapper around
[`live_connection()`](https://majerr.github.io/sqlhelper/reference/live_connection.md)
and
[`get_default_conn_name()`](https://majerr.github.io/sqlhelper/reference/set_default_conn_name.md)

## Usage

``` r
default_conn()
```

## Value

A database connection returned by
[`DBI::dbConnect()`](https://dbi.r-dbi.org/reference/dbConnect.html) or
[`pool::dbPool()`](http://rstudio.github.io/pool/reference/dbPool.md)

## Examples

``` r
library(sqlhelper)

connect(
    system.file(
        "examples/sqlhelper_db_conf.yml",
        package="sqlhelper"
        ),
    exclusive=TRUE
   )

default_conn()
#> <SQLiteConnection>
#>   Path: 
#>   Extensions: TRUE
```
