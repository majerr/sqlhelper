# sqlhelper
Easier SQL interaction with data

## Summary
`sqlhelper` does two things:

1. It **manages your connection to the CDS database**. It is opened automatically when the package is loaded.
1. It **provides functions for running sql queries and files of sql queries**. You simply provide a query or filename (or list of queries or filenames).

## Quickstart

### Running sql files and queries

For data wrangling, you probably want to write your queries in their own files. Then you will want to use `runfiles()`:

```R
# load sqlhelper
library(sqlhelper)

# write some queries
files_to_run <- list("path/to/create_temporary_dataset.sql",
                     "path/containing/final_merge.sql",
                     "folder/enclosing/extract_subsets.sql")

# Run the queries and save the results
results <- runfiles("cds",files_to_run)

# Inspect the results. runfiles() returns a list of lists.
# Names in the outer list are the filenames, stripped of path and extension:
my_subsets <- results$extract_subsets

# the results of each query in the file can be accessed by it's index:
head(my_subsets[[1]])

# queries that do not return a result (e.g. create table) will result in an empty list.
```

But sometimes you just want to run some short queries; in this case `runqueries()` is for you:

```R
# load sqlhelper
library(sqlhelper)

# write some queries
my_queries <- list("use COVID19", showtabs="select * from INFORMATION_SCHEMA.TABLES")

# Run the queries and save the results
results <- runqueries("hive",my_queries)

# Inspect the results. runqueries() returns a list with one element per query.
# You can access them using the names of the queries:
head(results$showtabs)

# or you can use indices (see below for comments on indexing lists in R):
head(results[[2]])
```


