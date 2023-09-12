## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
# load sqlhelper
library(sqlhelper)

# If your admin has kindly provided a site-wide config, or you have set one up 
# in your home directory, you can connect to your all your databases with:
# connect()

# Otherwise, you'll need to provide a config file:
connect("configs/sqlhelper_db_conf.yml")

## -----------------------------------------------------------------------------
library(sqlhelper)

connect("configs/sqlhelper_db_conf.yml")

sqlite_con <- live_connection("simple_sqlite")
DBI::dbWriteTable(sqlite_con,
                  name = "IRIS", 
                  value = iris)

n_longest_petals <- 6

readLines("sql/example.sql") |>
  writeLines()

result <- runfiles("sql/example.sql")

result$how_many_irises

result$n_longest_setosa_petal_lengths

