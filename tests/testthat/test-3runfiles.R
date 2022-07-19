library(sqlhelper)

cdsfn <- testthat::test_path("testfiles","test_cds.sql")
foofn <- testthat::test_path("testfiles","test_foo.sql")

test_that("test files exist",{
  expect_true(file.exists(cdsfn))
  expect_true(file.exists(foofn))
})


test_that("interpret_comments obtains the correct parameters", {
  reconnect(test_path("testfiles","sqlhelper_db_conf.yml"))
  interpreted_comments <- interpret_comments(readLines(foofn))
  expect_type(interpreted_comments,"list")
  expect_equal(interpreted_comments$conn,"pool_mem")
  expect_equal(interpreted_comments$qnames,c("showtabs"))

})



test_that("runfiles() will name elements of the returned list with their corresponding filenames",{
  reconnect(test_path("testfiles","sqlhelper_db_conf.yml"))
  iris2 <- iris
  iris2$flower_id <- rownames(iris2)
  dbWriteTable(live_connection("single_mem"),"iris",iris2)
  results <- runfiles(c(cdsfn, foofn))
  expected_result_names <- c("test_cds", "test_foo")
  expect_equal(names(results), expected_result_names)
})

test_that("runfiles() will run sequential queries and return a list of results", {

  reconnect(test_path("testfiles","sqlhelper_db_conf.yml"))
  iris2 <- iris
  iris2$flower_id <- rownames(iris2)
  dbWriteTable(live_connection("single_mem"),"iris",iris2)
  results <- runfiles(c(cdsfn, foofn))

  expect_type(results$test_cds,"list")
  expect_equal(length(results$test_cds), 3)
  expect_type(results$test_cds$showtabs,"list")
  expect_type(results$test_cds$sample,"list")
  expect_equal(nrow(results$test_cds$quoted_doubledash), 0)

})




