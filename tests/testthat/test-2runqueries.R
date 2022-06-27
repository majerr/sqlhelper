library(sqlhelper)
library(DBI)

test_that("runqueries() will run sequential queries and return a list of results using dbConnect", {
  reconnect(test_path("testfiles","sqlhelper_db_conf.yml"))
  dbWriteTable(live_connection("mem"),"iris",iris)
  queries <- list(cols = "PRAGMA table_info(iris)", top10 = "select * from iris limit 10")
  results <- runqueries(queries) # db parameter omitted to test default option
  expect_equal(length(results), 2)
  expect_equal(names(queries),names(results))
  expect_is(results$top10,"data.frame")
})

test_that("runqueries() will run sequential queries and return a list of results using Pool", {
  reconnect(test_path("testfiles","sqlhelper_db_conf.yml"),use_pool = TRUE)
  dbWriteTable(live_connection("mem"),"iris",iris)
  queries <- list(cols = "PRAGMA table_info(iris)", top10 = "select * from iris limit 10")
  results <- runqueries(queries) # db parameter omitted to test default option
  expect_equal(length(results), 2)
  expect_equal(names(queries),names(results))
  expect_is(results$top10,"data.frame")
  reconnect(test_path("testfiles","sqlhelper_db_conf.yml"))
})
