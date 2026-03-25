# Executing SQL

`sqlhelper`’s main purpose is the execution of *files* of SQL, with
options for controlling the execution of individual SQL queries within
each file. The function
[`run_files()`](https://majerr.github.io/sqlhelper/reference/run_files.md)
executes .SQL files. Internally,
[`run_files()`](https://majerr.github.io/sqlhelper/reference/run_files.md)
calls the functions
[`read_sql()`](https://majerr.github.io/sqlhelper/reference/read_sql.md),
[`prepare_sql()`](https://majerr.github.io/sqlhelper/reference/prepare_sql.md)
and
[`run_queries()`](https://majerr.github.io/sqlhelper/reference/run_queries.md)
and these functions can be used to read and prepare SQL files without
executing them, and to execute SQL query strings.

## Executing SQL files

Executing SQL code requires a connection to a database, and `sqlhelper`
provides ways to automate creating and managing connections. These are
described in
[`vignette("connections")`](https://majerr.github.io/sqlhelper/articles/connections.md).

Once connections are configured, the
[`run_files()`](https://majerr.github.io/sqlhelper/reference/run_files.md)
command can be used to execute files of SQL code.

``` r
library(sqlhelper)
connect("examples/sqlhelper_db_conf.yml", exclusive = TRUE)
DBI::dbWriteTable( default_conn(), name = "IRIS", iris)

readLines("examples/example.sql") |>
writeLines()
#> -- qname = how_many_irises
#> SELECT count(*) as N FROM IRIS;
#> 
#> -- qname = short_petal_setosa
#> select Species, `Petal.Length`
#> FROM IRIS
#> WHERE Species = "setosa"
#> AND `Petal.Length` < {petal_length}

petal_length <- 1.3

results <- run_files("examples/example.sql")

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
```

As well as individual file names,
[`run_files()`](https://majerr.github.io/sqlhelper/reference/run_files.md)
accepts a vector of file names.

### Accessing results of queries

[`run_files()`](https://majerr.github.io/sqlhelper/reference/run_files.md)
returns a list of results of the same length as the number of queries.
In the above example, names are assigned to queries with *interpreted
comments*, of the form `-- qname = my_query_name`. If queries are named,
individual results can be accessed by the same name:

``` r
results$short_petal_setosa
#>   Species Petal.Length
#> 1  setosa          1.1
#> 2  setosa          1.2
#> 3  setosa          1.0
#> 4  setosa          1.2
```

Results returned as list may also be accessed by index, of course.
However, if a file contains a single query, the result of that query
will be returned as is, (i.e. an object, not a single element list).

Beware of the usual gotchas around list names. `sqlhelper` will not
complain if you give two queries the same name, but if you then try to
access the results by name, you will only get the result of the first
query with that name. This is particularly relevant if your project
executes queries from multiple files and those files are developed by
different people. Similarly, be careful not to use anything in query
names that that R will interpret as an operator or special character. In
the above example, naming the query `short-petal-setosa` will cause an
error because R will interpret `-` as a subtraction.

### Controlling execution of individual queries

As well as naming queries, interpreted comments can be used to control
aspects of execution on a per-query basis. For example, queries are
executed with
[`DBI::dbGetQuery()`](https://dbi.r-dbi.org/reference/dbGetQuery.html)
by default, but `sqlite` will complain if you use this to send a
statement to the database. You can control the execution function with
the `execmethod` keyword:

``` sql
-- qname = bobby_tables
-- execmethod = sendq
DROP TABLE Students;
```

All interpreted comments follow the form `-- <keyword> = <value>`.

Interpretable keywords are:

- **qname** A name for this query
- **interpolate** “yes” or “no” - should this query be parameterized?
- **quotesql** “yes” or “no” - should interpolated characters be quoted?
- **execmethod** One of “get”, “execute”, “sendq”, “sends” or
  “spatial” - which method should be used to execute the query? “get”
  means
  [`DBI::dbGetQuery()`](https://dbi.r-dbi.org/reference/dbGetQuery.html);
  “execute” means
  [`DBI::dbExecute()`](https://dbi.r-dbi.org/reference/dbExecute.html);
  “sendq” means
  [`DBI::dbSendQuery()`](https://dbi.r-dbi.org/reference/dbSendQuery.html);
  “sends” means
  [`DBI::dbSendStatement()`](https://dbi.r-dbi.org/reference/dbSendStatement.html);
  “spatial” means
  [`sf::st_read()`](https://r-spatial.github.io/sf/reference/st_read.html).
- **geometry** The name of a spatial column. Ignored if `execmethod` is
  not ‘spatial’
- **conn_name** The name of a connection to execute this query against

``` r
## add combined standard/spatial example
```

#### Cascaded execution parameters

All interpreted comments except `qname` are cascaded *within their
file*, meaning that if you want to use the same values throughout, you
need only set them for the first query. See also
[`read_sql()`](https://majerr.github.io/sqlhelper/reference/read_sql.md)
for details.

If you want to change the execution parameters for the first query only
and retain the defaults for the remainder you will need to either:

- use interpreted comments to explicitly reset the defaults for the
  second query; or
- put the second and subsequent queries in a different file.

You can prevent cascading by passing `cascade = FALSE`.

``` r
## cascaded comments example
```

### Interpolation

By default, `sqlhelper` will attempt to parameterize SQL queries with
[`glue::glue_sql()`](https://glue.tidyverse.org/reference/glue_sql.html)
using values from the current environment. This means that values from R
can be easily inserted in your SQL code, or calculated in situ:

``` r
readLines("examples/petal_length_params.sql") |>
  writeLines()
#> -- qname = short_petal_setosa
#> select Species, `Petal.Length`
#> FROM IRIS
#> WHERE Species = "setosa"
#> AND `Petal.Length` < {petal_length};
#> 
#> -- qname = setosa_petal_1sd_below
#> select Species, `Petal.Length`
#> FROM IRIS
#> WHERE Species = "setosa"
#> AND `Petal.Length` < {
#>   mean(iris$Petal.Length[iris$Species == "setosa"])
#>     -  sd(iris$Petal.Length[iris$Species == "setosa"])
#> }

petal_length = 1.2

run_files("examples/petal_length_params.sql")
#> $short_petal_setosa
#>   Species Petal.Length
#> 1  setosa          1.1
#> 2  setosa          1.0
#> 
#> $setosa_petal_1sd_below
#>   Species Petal.Length
#> 1  setosa          1.1
#> 2  setosa          1.2
#> 3  setosa          1.0
#> 4  setosa          1.2
```

Interpolation behaviour can be controlled using the keywords
**interpolate** and **quotesql**, and the `values` parameter of
[`prepare_sql()`](https://majerr.github.io/sqlhelper/reference/prepare_sql.md)
(which can be also passed to
[`run_files()`](https://majerr.github.io/sqlhelper/reference/run_files.md)
and
[`run_queries()`](https://majerr.github.io/sqlhelper/reference/run_queries.md)).

The default behaviour is to quote SQL strings (i.e. interpolate with
[`glue::glue_sql()`](https://glue.tidyverse.org/reference/glue_sql.html));
if this is not desired it can be avoided with `-- quotesql = no` (for an
individual query in a file; but see [the ‘cascaded comments’ example,
above](#cascade)) or by passing `quotesql = "no"` as a parameter to
[`run_files()`](https://majerr.github.io/sqlhelper/reference/run_files.md)
(for all queries). If strings are not quoted they will be inserted bare;
whilst this is occasionally useful, great care should be taken to
sanitize any interpolated values.

If you want to skip interpolation for an individual query, precede it
with `-- interplate = no`. If you want to skip interpolation altogether,
pass `interpolate = "no"` as a parameter and see also [the ‘cascaded
comments’ example, above](#cascade).

#### Passing parameter values

Sometimes you may need to parameterize your SQL with values that are not
in the calling environment. This is particularly important if your are
executing SQL code from within a package: you cannot rely on, and should
not risk writing to, your users’
[`globalenv()`](https://rdrr.io/r/base/environment.html). To supply
interpolation values to
[`run_files()`](https://majerr.github.io/sqlhelper/reference/run_files.md)
and
[`run_queries()`](https://majerr.github.io/sqlhelper/reference/run_queries.md),
pass a populated environment as the `values` parameter.

``` r
# reusing the petal length parameter example
# A user may have a petal_length parameter in the globalenv already
print(petal_length)
#> [1] 1.2
result_from_globalenv <-
  run_files("examples/petal_length_params.sql")
result_from_globalenv$short_petal_setosa
#>   Species Petal.Length
#> 1  setosa          1.1
#> 2  setosa          1.0

# a bespoke environment can provide a specific set of values for interpolation
my_values <- new.env()
my_values$petal_length <- 1.4
result_from_my_values <-
  run_files("examples/petal_length_params.sql", values = my_values)
result_from_my_values$short_petal_setosa
#>    Species Petal.Length
#> 1   setosa          1.3
#> 2   setosa          1.1
#> 3   setosa          1.2
#> 4   setosa          1.3
#> 5   setosa          1.0
#> 6   setosa          1.2
#> 7   setosa          1.3
#> 8   setosa          1.3
#> 9   setosa          1.3
#> 10  setosa          1.3
#> 11  setosa          1.3
```

### Binding

Binding can be performed alongside interpolation. Queries and statements
are first interpolated and then should then be executed with
[`DBI::dbSendQuery()`](https://dbi.r-dbi.org/reference/dbSendQuery.html)
or
[`DBI::dbSendStatement()`](https://dbi.r-dbi.org/reference/dbSendStatement.html).
They may then be bound and the result fetched.

``` r
readLines("examples/binding.SQL") |>
  writeLines()
#> -- execmethod = sendq
#> -- qname = binding_example
#> SELECT species, [Petal.Width]
#> FROM IRIS
#> WHERE SPECIES = ? AND
#> [Petal.Width] < {petal_width};

petal_width <- 0.2

result <- run_files("examples/binding.SQL")

DBI::dbBind(result$binding_example, list("setosa"))

DBI::dbFetch(result$binding_example)
#>   Species Petal.Width
#> 1  setosa         0.1
#> 2  setosa         0.1
#> 3  setosa         0.1
#> 4  setosa         0.1
#> 5  setosa         0.1

DBI::dbClearResult(result$binding_example)
```

## Reading and preparing SQL files

SQL files and strings can be read and prepared without being executed by
the
[`read_sql()`](https://majerr.github.io/sqlhelper/reference/read_sql.md)
and
[`prepare_sql()`](https://majerr.github.io/sqlhelper/reference/prepare_sql.md)
functions. These functions return tibbles containing the prepared SQL,
associated metadata (e.g. filename), and execution parameters. These
functions enable both inspection of prepared SQL and parameters for
debugging, and further manipulation of SQL queries prior to execution.

## Executing SQL strings

One of the main objectives of `sqlhelper` is to reduce the incidence of
SQL written as strings in R code. However, it is occasionally convenient
for interactive exploratory work to type a query as a string. For this
you may use
[`run_queries()`](https://majerr.github.io/sqlhelper/reference/run_queries.md).
This function can also be used to execute queries that have been read
from files (e.g. with
[`read_sql()`](https://majerr.github.io/sqlhelper/reference/read_sql.md))
and then manipulated programmatically before execution.

## Passing ad-hoc connections to functions

It may not always be possible or desirable to have `sqlhelper` manage
your database connections. For example, the use of secrets is not yet
supported in `sqlhelper` connections. In these cases, connections
created outside `sqlhelper` may be passed to to
[`run_files()`](https://majerr.github.io/sqlhelper/reference/run_files.md)
or
[`run_queries()`](https://majerr.github.io/sqlhelper/reference/run_queries.md).

``` r

con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")

cars <- mtcars
cars$model <- row.names(mtcars)
DBI::dbWriteTable(con, "cars", cars)

minmpg = 30

run_queries("SELECT model, mpg, cyl FROM CARS WHERE mpg >= {minmpg}",
            default.conn = con)
#>            model  mpg cyl
#> 1       Fiat 128 32.4   4
#> 2    Honda Civic 30.4   4
#> 3 Toyota Corolla 33.9   4
#> 4   Lotus Europa 30.4   4
```
