test_that("runqueries runs queries in sequence", {
  sqlhelper::connect( testthat::test_path("testfiles",
                                     "sqlhelper_db_conf.yml"),
           exclusive = TRUE)

  DBI::dbWriteTable(live_connection("pool_mem"),DBI::SQL("iris"),iris)

  queries <- read_sql_file(
    testthat::test_path("testfiles",
                        "sequential_queries.sql")
    )


  expect_identical(DBI::dbGetQuery(live_connection("pool_mem"),
                               "SELECT COUNT(*) FROM IRIS3"),
                   head(iris,10))
})
