# sqlhelper
Easier SQL integration with R

## Summary
`sqlhelper` has two aims:

1. To make it easier for analysts who are not programmers to use SQL from R.
1. To make it easier for analysts who are programmers to use SQL from R in larger, more complex projects such as Shiny apps and packages.

To do this, `sqlhelper` does three things:

1. It **manages connections to RDBMSs**. Multiple DBI connections are configured with YAML and stored in the background for easy access by name.
1. It **provides functions for preparing and executing sql queries and files of sql queries**. You simply provide a query or filename (or lists of queries or filenames).
1. It **provides helper functions to facilitate interactive use and defensive programming**, e.g. to check a connection is live before executing some sql.

There are, of course, many excellent ways to execute code against a database
from R, often not involving SQL at all (e.g. [dbplyr](https://dbplyr.tidyverse.org/));
`sqlhelper` is for the times when you *do* want to use SQL - perhaps you have
inherited some legacy SQL, or need some specific functionality offered by your 
RDBMS, or simply prefer to write SQL.

## Installation

`sqlhelper` is under development and has not yet been released.

You can install the development version from github:

```R
# install.packages("devtools")
devtools::install_github("majerr/sqlhelper@dev")
```

## Usage

See https://majr-red.github.io/sqlhelper/dev
