test_that("get returns the default default", {
  connect(config_filename = testthat::test_path("testfiles",
                                                "sqlhelper_db_conf.yml"),
          exclusive=TRUE)

  expect_equal(get_default_conn_name(), "single_mem")
})

test_that("set errors if no known connection is named", {
  expect_error(set_default_conn_name("foo"))
})

test_that("set changes the default connection", {
  set_default_conn_name("pool_mem")
  expect_equal(get_default_conn_name(), "pool_mem")
  expect_true(connection_info("pool_mem")$default)
})

