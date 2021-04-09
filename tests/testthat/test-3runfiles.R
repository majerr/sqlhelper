context("runfiles")
library(ucsqlhelper)

hfn <- "../../sql/test_hive.sql"
pfn <- "../../sql/test_pg.sql"

test_that("interpret_comments obtains the correct parameters", {
  # for hive
  hive_interpreted_comments <- interpret_comments(readLines(hfn))
  expect_is(hive_interpreted_comments,"list")
  expect_equal(hive_interpreted_comments$db,"hIve")
  expect_equal(hive_interpreted_comments$qnames,c("usedb","showtabs","sample","set_hivevar","test_hivevar"))

  #for postgres
  pg_interpreted_comments <- interpret_comments(readLines(pfn))
  expect_is(pg_interpreted_comments,"list")
  expect_equal(pg_interpreted_comments$db,"PostgreSQL")
  expect_equal(pg_interpreted_comments$qnames,c("dbnames","tabnames"))
})

results <- runfiles(c(hfn,pfn))

test_that("runfiles() will name elements of the returned list with their corresponding filenames",{
  expected_result_names <- c()
  if(is.connected('h')){
    expected_result_names <- c(expected_result_names,"test_hive")
  }

  expected_result_names <- c(expected_result_names,"test_pg")
  expect_equal(names(results), c("test_hive","test_pg"))
})

test_that("runfiles() will run sequential queries on Hive and return a list of results", {
  if(not.connected('h')){
    skip("Hive is not available")
  }
  expect_is(results$test_hive,"list")
  expect_equal(length(results$test_hive), 5)
  expect_is(results$test_hive$showtabs,"data.frame")
  expect_is(results$test_hive$sample,"data.frame")
})

test_that("runfiles() will correctly identify both interpolated parameters and preset hivevars on hive",{
  if(not.connected('h')){
    skip("Hive is not available")
  }
  expect_equal(nrow(results$test_hive$test_hivevar), 310)
  expect_equal(max(as.Date(results$test_hive$test_hivevar$contract_dim_v.start_date)),as.Date("2017-03-31"))
})

test_that("runfiles() will run sequential queries on PostgreSQL and return a list of results", {
  if(not.connected('p')){
    skip("Postgres is not available")
  }
  expect_is(results$test_pg,"list")
  expect_equal(length(results$test_pg), 2)
  expect_is(results$test_pg$tabnames,"data.frame")
})



