# Browse available connections

Provides information about created connections.

## Usage

``` r
connection_info(name_str = ".*", exact = TRUE)
```

## Arguments

- name_str:

  A regular expression to be used to identify connection names to
  include. The default ('.\*') returns all of them.

- exact:

  TRUE or FALSE. Should `name_str` match the name of a connection
  exactly? TRUE will identify only 1 connection if name_str does not
  contain any metacharacters

## Value

Null, or a tibble with 1 row per identified connection and the following
fields:

- name:

  identifier (character)

- description:

  a description of the connection, if found in the conf file (character)

- live:

  is this connection valid and live? (logical)

- driver:

  the name of the driver function (character)

- conn_str:

  the string used to parameterize the connection (character)

- pool:

  is this a pool connection? (logical)

If no connection names matched `name_str`, the tibble will be empty. If
no connections have been configured (e.g.
[`connect()`](https://majerr.github.io/sqlhelper/dev/reference/connect.md)
has not been called), `NULL` is returned.

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

 connection_info()
#> # A tibble: 2 × 7
#>   name          description                  live  default driver conn_str pool 
#>   <chr>         <chr>                        <lgl> <lgl>   <glue> <chr>    <lgl>
#> 1 pool_sqlite   A pooled connection to an i… TRUE  FALSE   RSQLi… NA       TRUE 
#> 2 simple_sqlite A simple connection to an i… TRUE  TRUE    RSQLi… NA       FALSE

 connection_info("pool_sqlite")
#> # A tibble: 1 × 7
#>   name        description                    live  default driver conn_str pool 
#>   <chr>       <chr>                          <lgl> <lgl>   <glue> <chr>    <lgl>
#> 1 pool_sqlite A pooled connection to an in-… TRUE  FALSE   RSQLi… NA       TRUE 
```
