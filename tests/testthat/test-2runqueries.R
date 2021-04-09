context("runqueries")
library(ucsqlhelper)

test_that("runqueries() will run sequential queries and return a list of results (on hive)", {
  # Only one of hive or hive2 will be available at once
  if(not.connected('h')){
    skip("Hive is not available")
  }
  queries <- list(usedb = "use uc", showtabs = "show tables")
  results <- runqueries("h", queries)
  expect_equal(length(results), 2)
  expect_equal(names(queries),names(results))
  expect_is(results$showtabs,"data.frame")
})



