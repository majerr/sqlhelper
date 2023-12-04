# sqlhelper
Easier SQL integration with R

<p style="color:red"> This readme and the vignettes are currently under development. Expect broken links and inconsistency. The function documentation is cannon.</p>

## Summary

`sqlhelper` facilitates preparing and executing _files_ of SQL code from R.

In particular, `sqlhelper` does two things:

1. It **provides functions for preparing and executing sql queries and files of
sql queries**. You simply provide a query or filename (or lists of queries or
filenames). 
1. It **manages connections to RDBMSs**. Multiple DBI connections
can be configured with YAML and stored in the background for easy access by
name.

There are, of course, many excellent ways to interact with a database from R,
often not involving SQL at all (e.g. [dbplyr](https://dbplyr.tidyverse.org/));
`sqlhelper` is for the times when you *do* want to use SQL - perhaps you have
inherited some legacy SQL, or need some specific functionality offered by your
RDBMS, or simply prefer to write SQL. The article `vignette("use-cases")`
describes the setup for some specific motivating cases.

## Installation

A stable version of `sqlhelper` has not yet been released.

You can install the development version from github:

```R
# install.packages("devtools")
devtools::install_github("majerr/sqlhelper@dev")
```

## Quickstart

If you have a `yaml` connection file in your connection search path, you can simply do:

```r
library(sqlhelper)
results <- run_files(
  c(
    "path/to/extract.SQL",
    "folder/with/transform.SQL",
    "uri/of/load.SQL")
  )
```

If not, you will need to provide one:

```r
library(sqlhelper)
connect("connections.yml")

# now execute run_files(...) as above
```

For more on connection files, the search path, and managing connections see `vignette("connections")`. 

For more on executing SQL see `vignette("execution")`.


<!-- badges: start -->
  [![R-CMD-check](https://github.com/majerr/sqlhelper/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/majerr/sqlhelper/actions/workflows/R-CMD-check.yaml)
  [![codecov](https://codecov.io/gh/majerr/sqlhelper/graph/badge.svg?token=24TM252NTZ)](https://codecov.io/gh/majerr/sqlhelper)
  <!-- badges: end -->
