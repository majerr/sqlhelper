# Using sqlhelper in packages

`sqlhelper` was written to streamline SQL integration in packages
designed to meet organizations’ particular analytical needs, in stable
data contexts. The pattern of integrating SQL with R code in analysis
packages is intended to:

- make use of the data-wrangling capacity of databases and SQL;
- take advantage of R’s packaging system to write reliable,
  understandable code;
- keep all the code for a task in one place.

This note describes how to use `sqlhelper` safely inside a package.

## Connections

If more than one connection is needed for a project, the easiest
approach will be to define them in a yaml file as described in
[`vignette("connections")`](https://majerr.github.io/sqlhelper/articles/connections.md).
The yaml file is placed under `inst` in the root of the package and
accessed with `devtools::system.file()` (see
[r-pkgs](https://r-pkgs.org/misc.html#sec-misc-inst) on using installed
files).

``` r
sqlhelper::connect( 
    system.file( "sqlhelper_connection_conf.yml" ),
    exclusive=TRUE
)
```

Connections that cannot be defined this way may be defined within the
package. How this is handled will depend on considerations such as how
long each connection will be needed for, how widely it will need to be
shared between other package components, and whether it will need to be
exposed to users.

If a connection is needed once and may be used and closed during the
execution of a single function it may be sufficient to define it within
the namespace of that function and close it on or before the function
exits, for example:

``` r
get_some_data <- function(){
  conn <- DBI::dbConnect(a_driver, "a connection string")
  d <- sqlhelper::run_files( 
          system.file("SQL/my_sql_file.SQL"),
          default.conn = conn
        )
    
  DBI::dbDisconnect(conn)
  d
}
```

A connection that needs to be shared across functions or function calls,
but not exposed to users, may be stored in an environment in the
package’s top-level namespace, for example:

``` r
assign("connection_store",
       new.env(parent = emptyenv()),
       environment())
       
connect <- function(){
  assign(
    "c1", 
    DBI::dbConnect(a_driver, "a connection string"), 
    envir = connection_store
  )
}

get_some_data <- function(){
  sqlhelper::run_files( 
          system.file("SQL/my_sql_file.SQL"),
          default.conn = connection_store$c1
        )
}

# This is a bit belt-and-braces, but thorough.
disconnect <- function(){
  DBI::dbDisconnect(connection_store$c1)
  connection_store$c1 <- NULL
  rm(list=c("c1), envir=connection_store)
}
```

(this approach is in fact more or less the one take by `sqlhelper`
itself)

## SQL files

If multiple connections are required but cannot be defined in yaml it
will somewhat diminish the ability to control execution on a
query-by-query basis and may produce a need to split queries into more
files than would otherwise be necessary. mf a package contains many SQL
files, it may be useful to store them in a SQL directory under `inst/`.
They may be accessed in the same way, with
[`system.file()`](https://rdrr.io/r/base/system.file.html), either
`system.file("SQL", "file_name.SQL")` or
`system.file("SQL/file_name.SQL")` (the latter *may* be somewhat less
portable).

It is often convenient to define SQL parameters in a different scope to
the calling scope of
[`run_files()`](https://majerr.github.io/sqlhelper/reference/run_files.md).
In this case it is important to ensure that the package does not
interfere with the user’s global environment. The easiest way to do this
is to store them in an environment (e.g. in the same way as illustrated
for connections, above) and pass them to the `values` parameter of
[`run_files()`](https://majerr.github.io/sqlhelper/reference/run_files.md),
[`run_queries()`](https://majerr.github.io/sqlhelper/reference/run_queries.md)
or
[`prepare_sql()`](https://majerr.github.io/sqlhelper/reference/prepare_sql.md).
See
[`run_queries()`](https://majerr.github.io/sqlhelper/reference/run_queries.md)
or
[`vignette("execution")`](https://majerr.github.io/sqlhelper/articles/execution.md)
for examples.

## Exit

It may be desirable to close `sqlhelper`’s connections when a calling
package has completed it’s operations. This can be achieved easily with
[`sqlhelper::disconnect()`](https://majerr.github.io/sqlhelper/reference/disconnect.md).
