# sqlhelper
Easier SQL integration with R

## Summary
`sqlhelper` does three things:

1. It **manages connections to RDBMSs**. Multiple DBI connections are configured with YAML and stored in the background for easy access by name.
1. It **provides functions for preparing and executing sql queries and files of sql queries**. You simply provide a query or filename (or lists of queries or filenames).
1. It **provides helper functions to facilitate interactive use and defensive programming**, e.g. to check a connection is live before executing some sql.

There are, of course, many excellent ways to execute code against a database
from R, often not involving SQL at all (e.g. [dbplyr](https://dbplyr.tidyverse.org/));
`sqlhelper` is for the times when you *do* want to use SQL - perhaps you have
inherited some legacy SQL, or need some specific functionality offered by your 
RDBMS, or simply prefer to write SQL.

## Installation

Install from github:

```R
# install.packages("devtools")
devtools::install_github("majr-red/sqlhelper")
```

## Usage

For data wrangling, you probably want to write SQL queries in their own files and use `runfiles()`:

```R
# load sqlhelper
library(sqlhelper)

# If your admin has kindly provided a site-wide config, or you have set up
# your own, you can connect to your all your databases with:
connect()

# Otherwise, you'll need to provide a config file 
# (see 'Getting Started' for what to put in it)
# connect("path/to/db_config_file.yml")

# write some queries
files_to_run <- list("path/to/create_temporary_dataset.sql",
                     "path/containing/final_merge.sql",
                     "folder/enclosing/extract_subsets.sql")

# Run the queries and save the results
results <- runfiles(files_to_run)

# Inspect the results. By default, runfiles() returns a list of the results of 
# each query in the files you provided. Results of a specific query can be accessed by the
# the name of the query. See 'Getting Started' for more on named queries.

head(results$my_extracted_subset)
```

For exploratory work you might just want to run some short queries; in this case `runqueries()` is for you:

```R
library(sqlhelper)
connect()

# write some queries
my_queries <- list(usedb="use COVID19", showtabs="select * from INFORMATION_SCHEMA.TABLES")

# Run the queries and save the results
results <- runqueries(my_queries)

# Inspect the results. runqueries() returns a list with one element per query.
# You can access them using the names of the queries:
head(results$showtabs)

# or you can use indices (see below for comments on indexing lists in R):
head(results[[2]])
```

