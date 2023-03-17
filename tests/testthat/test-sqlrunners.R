test_that("runqueries runs queries in sequence", {
  connect( testthat::test_path("testfiles",
                               "sqlhelper_db_conf.yml"),
           exclusive = TRUE)


  DBI::dbWriteTable(live_connection("pool_mem"),DBI::SQL("iris"),iris)

  queries <- read_sql(
    testthat::test_path("testfiles",
                        "sequential_queries.sql")
    )

  runqueries( queries )

  expect_identical(DBI::dbGetQuery(live_connection("pool_mem"),
                               "SELECT COUNT(*) FROM IRIS3")[[1,1]],
                   nrow(head(iris,10)))
})

test_that("runfiles runs files correctly", {
  results <- runfiles(
    c(
      testthat::test_path("testfiles",
                           "test_runfiles.sql"),
      testthat::test_path("testfiles",
                          "test_runfiles2.sql")
      )
  )

  expect_identical(results$two,
                   head(iris, 50) |> dplyr::mutate_if(is.factor,as.character))

  expect_identical(results$three,
                   head(iris, 10) |> dplyr::mutate_if(is.factor,as.character))
})

test_that("spatial read works", {
  skip("NOT RUN")
  DBI::dbWriteTable(live_connection("single_mem"),
                   "congruent",
                   spData::congruent)

  results <- runqueries(c("congruent"="SELECT * FROM congruent"),
                        default_conn = live_connection("single_mem"),
                        execmethod = "spatial",
                        geometry = "geometry")

  # This fails - the CRS changes during the write/read operation. Issue raised
  # with sf maintainers: https://github.com/r-spatial/sf/issues/2017
  # ... and closed without explanation. Must have been a stupid question.

  expect_equal(results$congruent,
                   spData::congruent)
})
