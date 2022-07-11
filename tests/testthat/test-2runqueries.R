library(sqlhelper)
library(DBI)

test_that("runqueries() will run sequential queries and return a list of results using dbConnect", {
  reconnect(test_path("testfiles","sqlhelper_db_conf.yml"))
  dbWriteTable(live_connection("single_mem"),"iris",iris)
  queries <- list(cols = "PRAGMA table_info(iris)", top10 = "select * from iris limit 10")
  results <- runqueries(queries) # db parameter omitted to test default option - should be 'single_mem'
  expect_equal(length(results), 2)
  expect_equal(names(queries),names(results))
  expect_s3_class(results$top10,"data.frame")
})

test_that("runqueries() will run sequential queries and return a list of results using Pool", {
  reconnect(test_path("testfiles","sqlhelper_db_conf.yml"))
  dbWriteTable(live_connection("pool_mem"),"iris",iris)
  queries <- list(cols = "PRAGMA table_info(iris)", top10 = "select * from iris limit 10")
  results <- runqueries(queries,db="pool_mem")
  expect_equal(length(results), 2)
  expect_equal(names(queries),names(results))
  expect_s3_class(results$top10,"data.frame")
  reconnect(test_path("testfiles","sqlhelper_db_conf.yml"))
})
