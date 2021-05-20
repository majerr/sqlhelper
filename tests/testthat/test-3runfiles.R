context("runfiles")
library(sqlhelper)

cdsfn <- "../../sql/test_cds.sql"
foofn <- "../../sql/test_foo.sql"

test_that("interpret_comments obtains the correct parameters", {

  interpreted_comments <- interpret_comments(readLines(foofn))
  expect_is(interpreted_comments,"list")
  expect_equal(interpreted_comments$db,"cDs")
  expect_equal(interpreted_comments$qnames,c("usedb"))

})

results <- runfiles(c(cdsfn, foofn))

test_that("runfiles() will name elements of the returned list with their corresponding filenames",{
  expected_result_names <- c("test_cds", "test_foo")
  expect_equal(names(results), expected_result_names)
})

test_that("runfiles() will run sequential queries on CDS and return a list of results", {
  if(not.connected('cds')){
    skip("CDS is not available")
  }
  expect_is(results$test_cds,"list")
  expect_equal(length(results$test_cds), 3)
  expect_is(results$test_cds$showtabs,"data.frame")
  expect_is(results$test_cds$sample,"data.frame")
})




