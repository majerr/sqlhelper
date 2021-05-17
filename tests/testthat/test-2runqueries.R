context("runqueries")
library(sqlhelper)

test_that("runqueries() will run sequential queries and return a list of results", {
  # Only one of hive or hive2 will be available at once
  if(not.connected('cds')){
    skip("CDS is not available")
  }
  queries <- list(usedb = "use COVID19", showtabs = "select * from INFORMATION_SCHEMA.TABLES")
  results <- runqueries(queries) # db parameter omitted to test default option
  expect_equal(length(results), 2)
  expect_equal(names(queries),names(results))
  expect_is(results$showtabs,"data.frame")
})
