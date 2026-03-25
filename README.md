# sqlhelper
Easier SQL integration with R

## Summary

`sqlhelper` facilitates preparing and executing _files_ of SQL code from R.

In particular, `sqlhelper` does two things. It:

1. provides **functions for preparing and executing files of
sql queries**; and it
1. provides **functions to manage multiple RDBMS connections**. 

To do this `sqlhelper` defines a number of functions, many of which are also
exported for convenience.

There are, of course, many excellent ways to interact with a database from R,
often not involving SQL at all (e.g. [dbplyr](https://dbplyr.tidyverse.org/));
`sqlhelper` is for the times when you *do* want to use SQL - perhaps you have
inherited some legacy SQL, or need some specific functionality offered by your
RDBMS, or simply prefer to write SQL. The article `vignette("use-case")`
describes the setup for a specific motivating case.

## Installation

The current stable version is available from CRAN:

```R
install.packages("sqlhelper")
```

You can the install development version from github:

```R
# install.packages("devtools")

devtools::install_github("majerr/sqlhelper@dev")
```

## Getting Started

Basic functionality is described in [`vignette("sqlhelper")`](https://majerr.github.io/sqlhelper/articles/sqlhelper.html)

Execution of SQL files is described in detail in [`vignette("execution")`](https://majerr.github.io/sqlhelper/articles/execution.html)

Setup and management of database connections is described in
[`vignette("connections")`](https://majerr.github.io/sqlhelper/articles/connections.html)

There are some details about using `sqlhelper` within other packages in [`vignette("use_case")`](https://majerr.github.io/sqlhelper/articles/use_case.html)



