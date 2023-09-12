test_that("config must be a list", {
  expect_warning(validate_configs(list("c"="foo")),
                 "Connection configuration for c was invalid; c will not be available")

})

test_that("All invalidations are captured", {
  fn <- testthat::test_path("testfiles",
                            "validate_configs_test.yml")
  configs <- read_configs(fn,exclusive=TRUE)
  expect_warning(validate_configs(configs["no_connection"]),
                 "Connection configuration for no_connection was invalid; no_connection will not be available")
  expect_warning(validate_configs(configs["no_server_type"]),
                 "Connection configuration for no_server_type was invalid; no_server_type will not be available")
  expect_warning(validate_configs(configs["conn_child_is_list"]),
                 "Connection configuration for conn_child_is_list was invalid; conn_child_is_list will not be available")
  expect_warning(validate_configs(configs["no_server"]),
                 "Connection configuration for no_server was invalid; no_server will not be available")


  expect_false(validate_configs(configs["no_pool"])$no_pool$pool)
  expect_false(validate_configs(configs["pool_not_logical"])$pool_not_logical$pool)
  expect_false(validate_configs(configs["pool_too_long"])$pool_too_long$pool)

  })
