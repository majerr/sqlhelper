library(testthat)

op <- options("SQLHELPER_NO_CONNECT_ON_LOAD" = FALSE)
library(sqlhelper)

test_check("sqlhelper")

options(op)
