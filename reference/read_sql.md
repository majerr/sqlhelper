# Read a sql file and return it's contents as a tibble

Read a sql file and return it's contents as a tibble

## Usage

``` r
read_sql(file_name, cascade = TRUE)
```

## Arguments

- file_name:

  Full name and path of a file to read

- cascade:

  Parameters for executing each query may be specified as comments in
  the SQL file. If `cascade=TRUE`, execution parameters specified in the
  file will be cascaded to subsequent queries where that parameter is
  not specified. This enables you to set a parameter (e.g. the
  connection name) once, for the first query in a file, and use it for
  all the subsequent queries.

## Value

A tibble containing 1 row per query with the following fields:

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

  character. If `execmethod` is "spatial", which is the geometry column?

- conn_name:

  character. The name of the database connection to use for this query.
  Must be the name of a configured sqlhelper connection.

- sql:

  The sql query to be executed

- filename:

  The value of `file_name`

## Details

Multiple SQL queries in files should be terminated by semi-colons (`;`),
as usual.

The values of `qname`, `quotesql`, `interpolate`, `execmethod`,
`geometry`, and `conn_name` in the output may be specified with comments
immediately preceding each query (see examples).

With the exception of `qname`, the value of each interpreted comment is
cascaded to subsequent queries (assuming `cascade=TRUE`). This means you
may set values once for the first query in the file and they will apply
to all the queries thereafter.

See
[`run_queries()`](https://majerr.github.io/sqlhelper/reference/run_queries.md)
for the implications of setting execution parameters. See
[`prepare_sql()`](https://majerr.github.io/sqlhelper/reference/prepare_sql.md)
for the treatment of missing values in the output and their defaults.
The article
[`vignette("execution")`](https://majerr.github.io/sqlhelper/articles/execution.md)
has further examples of using these parameters to control execution.

## Examples

``` r
library(sqlhelper)

fn <- system.file( "examples/read_sql_execution_params.SQL",
                   package="sqlhelper" )
readLines( fn ) |> writeLines()
#> -- qname = create_setosa_table
#> -- execmethod = execute
#> -- conn_name = sqlite_simple
#> CREATE TABLE iris_setosa as SELECT * FROM IRIS WHERE SPECIES = 'setosa';
#> 
#> -- qname = get_setosa_table
#> -- execmethod = get
#> -- conn_name = sqlite_simple
#> SELECT * FROM iris_setosa;

sql_tibble <- read_sql(fn)
sql_tibble
#> # A tibble: 2 × 8
#>   qname        quotesql interpolate execmethod geometry conn_name sql   filename
#>   <chr>        <chr>    <chr>       <chr>      <chr>    <chr>     <chr> <chr>   
#> 1 create_seto… NA       NA          execute    NA       sqlite_s… CREA… read_sq…
#> 2 get_setosa_… NA       NA          get        NA       sqlite_s… SELE… read_sq…
sql_tibble$sql
#> [1] "CREATE TABLE iris_setosa as SELECT * FROM IRIS WHERE SPECIES = 'setosa'"
#> [2] "SELECT * FROM iris_setosa"                                              

fn <- system.file( "examples/read_sql_comments.SQL", package="sqlhelper" )
readLines( fn ) |> writeLines()
#> -- Run some queries against the newly-created iris table
#> 
#> 
#> -- conn_name = simple_sqlite
#> -- ------------------------------------------------------------
#> 
#> -- what tables have we got?
#> -- qname = showtabs
#> SELECT name FROM
#> 
#> /* a multi-
#> line comment in the
#> middle of the
#> block*/
#> 
#> sqlite_schema WHERE type='table';
#> 
#> -- ------------------------------------------------------------
#> 
#> -- qname=sample
#> select * -- with a comment on a code line
#> -- and another on a line by itself in the middle of a block
#> from iris limit 10;
#> 
#> /* a multi-
#> line comment at
#> the end of the
#> block*/
#> 
#> -- ------------------------------------------------------------
#> -- spaces are allowed in interpreted comments
#> -- conn_name = pool_sqlite
#> -- execmethod = spatial
#> -- geometry = myString
#> -- make sure quoted double dashes survive the comment strip
#> -- qname=quoted_doubledash
#> select
#> 'stringvar' as myString
#> from iris
#> where 'foo' = '-- oops';
#> 
#> /* a single-line block comment */
#> 
#> -- ------------------------------------------------------------
#> 
#> -- make sure quoted block comments survive the comment strip
#> -- 'conn_name', 'execmethod' and 'geometry' values can be
#> -- cascaded from the previous query
#> -- qname=quoted_block
#> select
#> 'stringvar' as myString
#> from iris
#> where 'foo' = ' /* KEEP ME! */ ';
#> 
#> -- ------------------------------------------------------------
#> 
#> 
#> -- a few single
#> -- line comments at the
#> -- very end

sql_tibble <- read_sql(fn)
sql_tibble
#> # A tibble: 4 × 8
#>   qname        quotesql interpolate execmethod geometry conn_name sql   filename
#>   <chr>        <chr>    <chr>       <chr>      <chr>    <chr>     <chr> <chr>   
#> 1 showtabs     NA       NA          NA         NA       simple_s… SELE… read_sq…
#> 2 sample       NA       NA          NA         NA       simple_s… sele… read_sq…
#> 3 quoted_doub… NA       NA          spatial    myString pool_sql… sele… read_sq…
#> 4 quoted_block NA       NA          spatial    myString pool_sql… sele… read_sq…
sql_tibble$sql
#> [1] "SELECT name FROM sqlite_schema WHERE type='table'"                        
#> [2] "select * from iris limit 10"                                              
#> [3] "select 'stringvar' as myString from iris where 'foo' = '-- oops'"         
#> [4] "select 'stringvar' as myString from iris where 'foo' = ' /* KEEP ME! */ '"
```
