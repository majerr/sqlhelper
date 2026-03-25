# sqlhelper

## Connections

Before you can execute any SQL you will need connections to the
databases you want to use. Either define them yourself (e.g. using
[`DBI::dbConnect()`](https://dbi.r-dbi.org/reference/dbConnect.html)),
or place their specifications in a yaml file and use
[`connect()`](https://majerr.github.io/sqlhelper/dev/reference/connect.md):

``` r
library(sqlhelper)

conf_fn <- "examples/sqlhelper_db_conf.yml"

readLines(conf_fn) |>
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

connect(conf_fn)
```

[`vignette("connections")`](https://majerr.github.io/sqlhelper/dev/articles/connections.md)
and
[`connect()`](https://majerr.github.io/sqlhelper/dev/reference/connect.md)
describe in detail how to specify and manage connections. In particular,
note that `sqlhelper` defines a configuration search path. This means
that if you tend to connect to the same databases in every session, you
can avoid supplying file names by placing yaml files in the search path.
Similarly, if you are a database admin you can supply connections to
your analysts by placing such a file in the site-wide directory of the
search path.

## Executing files

`sqlhelper`’s principal function is
[`run_files()`](https://majerr.github.io/sqlhelper/dev/reference/run_files.md);
use it to execute files or list of files. If you have used
[`connect()`](https://majerr.github.io/sqlhelper/dev/reference/connect.md)
to set up your connections you can simply pass file names to
[`run_files()`](https://majerr.github.io/sqlhelper/dev/reference/run_files.md).

``` r

# Write iris to sqlhelper's default connection
DBI::dbWriteTable(default_conn(), name = "IRIS", value = iris)

# write some queries in a .sql file
file_to_run <- "examples/example.sql"

readLines(file_to_run) |> writeLines()
#> -- qname = how_many_irises
#> SELECT count(*) as N FROM IRIS;
#> 
#> -- qname = short_petal_setosa
#> select Species, `Petal.Length`
#> FROM IRIS
#> WHERE Species = "setosa"
#> AND `Petal.Length` < {petal_length}

#Define a parameter
petal_length <- 1.3

# Run the queries and save the results
results <- run_files(file_to_run)

# Inspect the results. By default, run_files() returns a list of the results of 
# each query in the files you provided. Results of a specific query can be accessed by the
# the name of the query. See the article 'Executing SQL' for more on named queries.
results
#> $how_many_irises
#>     N
#> 1 150
#> 
#> $short_petal_setosa
#>   Species Petal.Length
#> 1  setosa          1.1
#> 2  setosa          1.2
#> 3  setosa          1.0
#> 4  setosa          1.2

results$short_petal_setosa
#>   Species Petal.Length
#> 1  setosa          1.1
#> 2  setosa          1.2
#> 3  setosa          1.0
#> 4  setosa          1.2
```

This example illustrates two key aspects of
[`run_files()`](https://majerr.github.io/sqlhelper/dev/reference/run_files.md).
Queries can be parameterized using the familiar
[`glue::glue_sql()`](https://glue.tidyverse.org/reference/glue_sql.html)
syntax, and queries can be named using interpreted comments for easier
access to the results. Other aspects of execution can also be controlled
using interpreted comments;
[`run_files()`](https://majerr.github.io/sqlhelper/dev/reference/run_files.md)
and
[`vignette("execution")`](https://majerr.github.io/sqlhelper/dev/articles/execution.md)
describe this in detail.

## Executing queries

For exploratory work you might just want to run some short queries
interactively. Use the function
[`run_queries()`](https://majerr.github.io/sqlhelper/dev/reference/run_queries.md)
for this.

``` r
# write some queries
my_queries <- c( 
  showtabs = "SELECT name FROM sqlite_schema WHERE type ='table' AND name NOT LIKE 'sqlite_%'",
  how_many_irises = "select count(*) from iris"
)

# Run the queries and save the results
results <- run_queries(my_queries)

# Inspect the results. runqueries() returns a list with one element per query.
# You can access them using the names of the queries:
results$showtabs
#>   name
#> 1 IRIS

results$how_many_irises
#>   count(*)
#> 1      150
```

[`run_queries()`](https://majerr.github.io/sqlhelper/dev/reference/run_queries.md)
and
[`vignette("execution")`](https://majerr.github.io/sqlhelper/dev/articles/execution.md)
describe in detail the options for running individual queries.

## Preparing queries

Prior to execution,
[`run_files()`](https://majerr.github.io/sqlhelper/dev/reference/run_files.md)
reads and prepares SQL files using the functions
[`read_sql()`](https://majerr.github.io/sqlhelper/dev/reference/read_sql.md)
and
[`prepare_sql()`](https://majerr.github.io/sqlhelper/dev/reference/prepare_sql.md)
respectively. These functions can be also be used in isolation to read
and/or prepare SQL for execution without actually executing it. This can
be useful if you need paramaterize your SQL in a differently (for
example by using
[`DBI::dbBind()`](https://dbi.r-dbi.org/reference/dbBind.html)), or if
you need to debug your parameterized SQL.
