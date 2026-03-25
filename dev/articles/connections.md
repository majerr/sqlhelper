# Managing Connections

`sqlhelper` can maintain one or many database connections internally.
This article describes how to configure and create connections, and the
functions for inspecting and accessing them.

## Creating connections

You can define database connections in config files which are read and
processed by the
[`connect()`](https://majerr.github.io/sqlhelper/dev/reference/connect.md)
function. For example, if your config file is called `my_db_config.yml`,
you might run:

``` r
library(sqlhelper)

connect("examples/sqlhelper_db_conf.yml")
```

### Config files

A config file can contain one or more connection definitions. The above
example defines two simple connections:

``` r
readLines("examples/sqlhelper_db_conf.yml") |>
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
```

Connection definitions are [YAML](https://yaml.org/). The following YAML
chunk defines a connection called ‘dap’.

``` yaml
dap:
  driver_type: sqlserver
  pool: yes
  description: >
    Databases managed by ADD teams on the Data and Analytics Platform
  connection:
    Driver: "{ODBC Driver 17 for SQL Server}"
    Server: "Dap-sql01"
    Trusted_Connection: "yes"
```

The first line is a name for this connection; subsequent lines contain
information about the connection.

The **driver_type** line defines the brand of database, and hence the
driver package that will be used. This parameter is required. Current
options are:

- ‘odbc’ (odbc)
- ‘sqlserver’ (odbc)
- ‘sqlite’ (RSQLite)
- ‘postgresql’ (RPostgres)
- ‘mariadb’(RMariaDB)
- ‘mysql’(RMariaDB)
- ‘bigquery’ (bigquery)

The **pool** line determines whether a single connection is required (as
returned by
[`DBI::dbConnect()`](https://dbi.r-dbi.org/reference/dbConnect.html)) or
a pool of connections (as returned by
[`pool::dbPool()`](http://rstudio.github.io/pool/reference/dbPool.md)).
The latter is recommended for [shiny](https://shiny.posit.co/)
applications. This parameter defaults to ‘no’, meaning a single
connection will be provided.

The **description** line provides a short description of this
connection. This parameter can be omitted.

The **connection** section contains several lines which provide the
parameters required for the connection itself, to be passed to the
constructor function as name=value pairs. For example, you will need to
supply a `Driver` connection string if you are using an `odbc` driver.
The **server** parameter will probably always be required but see
[`DBI::dbConnect()`](https://dbi.r-dbi.org/reference/dbConnect.html) for
more details.

In summary, the elements needed for every connection are:

1.  The connection **name**, and
2.  the **driver_type**, and
3.  a **connection** section containing a **Server** element

The others *may* be needed for some connections or may be omitted
entirely. The function
[`config_examples()`](https://majerr.github.io/sqlhelper/dev/reference/config_examples.md)
provides several example configurations.

### The config search path

By default, the
[`connect()`](https://majerr.github.io/sqlhelper/dev/reference/connect.md)
function reads not only the config file named by its `config_filename`
parameter, but also any config files found on a search path that
includes both the user’s and the site’s config directories.

If you use the same database connections often, you can place a config
file called `sqlhelper_db_config.yml` in the directory named by
[`rappdirs::user_config_dir()`](https://rappdirs.r-lib.org/reference/user_data_dir.html).
Similarly, if you are a site admin you can provide your users with
pre-configured connections by placing a config file of the same name in
the directory named by
[`rappdirs::site_config_dir()`](https://rappdirs.r-lib.org/reference/site_data_dir.html).
In either of these cases, no config filename is needed and
[`connect()`](https://majerr.github.io/sqlhelper/dev/reference/connect.md)
can be called without arguments. Nonetheless, in the case where you
require additional connections to those provided by user- and site-wide
configurations, you may provide a `config_filename` argument.

### Suppressing the config search path

If only one config file is wanted, the config search path may be
suppressed by supplying the `exclusive = TRUE` argument.

If `exclusive = TRUE`, then
[`connect()`](https://majerr.github.io/sqlhelper/dev/reference/connect.md)
will check whether the `config_filename` parameter is ‘user’ or ‘site’.
If `config_filename` is one of those, the user- or site-wide config file
will be sought; if it is not, it will be treated as the name of a config
file, and that file will be sought.

### File precedence, inheritance, and conflicts across config files

If `exclusive == FALSE`, site-wide config files will be read first, then
user-wide, then named files. If conflicting connection names or
connection elements are encountered between config files, values from
later files overwrite those from earlier. This mechanism operates
element-wise; for example, if your site-wide config contains:

``` yaml
my_conn:
  server_type: mysql
  connection:
    Server: "organization_server_host"
```

but your named file contains:

``` yaml
my_conn:
  description: Private MySQL database
  connection:
    Server: "my_alternate_server_host"
```

then `sqlhelper` will attempt to create a connection described by:

``` yaml
my_conn:
  server_type: mysql
  description: Private MySQL database
  connection:
    Server: "my_alternate_server_host"
```

## Browsing the connection cache

After
[`connect()`](https://majerr.github.io/sqlhelper/dev/reference/connect.md)
has been called, you may inspect the created connections with
[`connection_info()`](https://majerr.github.io/sqlhelper/dev/reference/connection_info.md).

``` r
connection_info()
#> # A tibble: 2 × 7
#>   name          description                  live  default driver conn_str pool 
#>   <chr>         <chr>                        <lgl> <lgl>   <glue> <chr>    <lgl>
#> 1 pool_sqlite   A pooled connection to an i… TRUE  FALSE   RSQLi… NA       TRUE 
#> 2 simple_sqlite A simple connection to an i… TRUE  TRUE    RSQLi… NA       FALSE
```

## Accessing individual connections

Connections may be accessed by name, with
[`live_connection()`](https://majerr.github.io/sqlhelper/dev/reference/live_connection.md)

``` r
myconn <- live_connection("simple_sqlite")
myconn
#> <SQLiteConnection>
#>   Path: 
#>   Extensions: TRUE
```

## The default connection

The first connection in the file with the highest precedence (i.e. the
last one to be read) becomes the default default. This connection will
be used whenever queries or files of SQL are executed
([`runqueries()`](https://majerr.github.io/sqlhelper/dev/reference/run_queries.md)
or
[`runfiles()`](https://majerr.github.io/sqlhelper/dev/reference/run_files.md))
with without explicitly stating which connection to use.

You can check the default connection with
[`connection_info()`](https://majerr.github.io/sqlhelper/dev/reference/connection_info.md)
or
[`get_default_conn_name()`](https://majerr.github.io/sqlhelper/dev/reference/set_default_conn_name.md)
and change it with
[`set_default_conn_name()`](https://majerr.github.io/sqlhelper/dev/reference/set_default_conn_name.md).

``` r
get_default_conn_name()
#> [1] "simple_sqlite"

set_default_conn_name("pool_sqlite")

get_default_conn_name()
#> [1] "pool_sqlite"

get_default_conn_name() |>
  live_connection()
#> <Pool> of SQLiteConnection objects
#>   Objects checked out: 0
#>   Available in pool: 1
#>   Max size: Inf
#>   Valid: TRUE
```

The last of these - combining
[`get_default_conn_name()`](https://majerr.github.io/sqlhelper/dev/reference/set_default_conn_name.md)
with
[`live_connection()`](https://majerr.github.io/sqlhelper/dev/reference/live_connection.md)
to obtain the default connection - is common enough to warrant a
convenience function,
[`default_conn()`](https://majerr.github.io/sqlhelper/dev/reference/default_conn.md).

## Checking, closing and re-opening connections

You can check whether connections are live in two ways:

1.  [`connection_info()`](https://majerr.github.io/sqlhelper/dev/reference/connection_info.md)
    provides a ‘live’ field for interactive use
2.  The functions
    [`is_connected()`](https://majerr.github.io/sqlhelper/dev/reference/is_connected.md)
    and
    [`not_connected()`](https://majerr.github.io/sqlhelper/dev/reference/is_connected.md)
    are intended for programmatic use.

To close all connections and remove them from the internal cache, use
[`disconnect()`](https://majerr.github.io/sqlhelper/dev/reference/disconnect.md).

Individual connections may be closed with
[`DBI::dbDisconnect()`](https://dbi.r-dbi.org/reference/dbDisconnect.html).

[`connect()`](https://majerr.github.io/sqlhelper/dev/reference/connect.md)
closes any open connections before reading config files.

``` r
conn_name <- "simple_sqlite"

is_connected(conn_name)
#> [1] TRUE
connection_info(conn_name)$live
#> [1] TRUE

myconn <- live_connection(conn_name)
DBI::dbDisconnect(myconn)

connection_info()
#> # A tibble: 2 × 7
#>   name          description                  live  default driver conn_str pool 
#>   <chr>         <chr>                        <lgl> <lgl>   <glue> <chr>    <lgl>
#> 1 pool_sqlite   A pooled connection to an i… TRUE  TRUE    RSQLi… NA       TRUE 
#> 2 simple_sqlite A simple connection to an i… FALSE FALSE   RSQLi… NA       FALSE

if(not_connected(conn_name)){
  message(glue::glue("{conn_name} is not available, reconnecting..."))
  connect("examples/sqlhelper_db_conf.yml", exclusive = TRUE)
}
#> simple_sqlite is not available, reconnecting...

connection_info()
#> # A tibble: 2 × 7
#>   name          description                  live  default driver conn_str pool 
#>   <chr>         <chr>                        <lgl> <lgl>   <glue> <chr>    <lgl>
#> 1 pool_sqlite   A pooled connection to an i… TRUE  FALSE   RSQLi… NA       TRUE 
#> 2 simple_sqlite A simple connection to an i… TRUE  TRUE    RSQLi… NA       FALSE

disconnect()

connection_info()
#> NULL
```
