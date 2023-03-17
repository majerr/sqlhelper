# TODO: redesign based on prepare_sql code - only test what's necessary here
#     : lots of this stuff has already been tested in test_read_sql


test_that("defaults are properly set", {
  connect(config_filename = testthat::test_path("testfiles",
                                                "sqlhelper_db_conf.yml"),
          exclusive=TRUE)

  # test default defaults
  sql <- prepare_sql(c(one="select 1"))

  expect_equal(sql$qname, "one")
  expect_equal(sql$quotesql, "yes")
  expect_equal(sql$interpolate, "yes")
  expect_equal(sql$execmethod, "get")
  expect_true(is.na(sql$geometry))
  expect_equal(sql$conn_name, "default")
  expect_equal(sql$sql, "select 1")
  expect_true(is.na(sql$filename))
  expect_equal(sql$prepared_sql[[1]], DBI::SQL("select 1"))

  # change defaults

})

# interpolation is correct
test_that("interpolation is correct", {

  # value not available
  expect_error(prepare_sql(c(one="select {n}")))

  # interpolate settings are obeyed: interpolate
  sql <- prepare_sql(c(one="select {n}"), values = NULL)
  expect_equal(sql$interpolate, "no")
  expect_false(sql$prepared_sql[[1]] == "select 3")

  n = 3
  sql <- prepare_sql(c(one="select {n}"))
  expect_true(sql$prepared_sql[[1]] == "select 3")

  # parent.env vs alt env
  env <- new.env()
  env$n <- 2
  sql <- prepare_sql(c(one="select {n}"), values = env)
  expect_true(sql$prepared_sql[[1]] == "select 2")

  # settings are obeyed: quotesql


})
