library(sqlhelper)

test_that("get_config reads appropriate config file", {

  expect_equal(
    names(
      read_config_file(
        testthat::test_path("testfiles",
                            "sqlhelper_db_conf.yml")
      )
    ),
    c("single_mem","pool_mem")
  )

})

test_that("get_all_configs appropriately combines config files", {
  skip_on_cran()
  conf <- get_all_configs(testthat::test_path("testfiles",
                                              "sqlhelper_db_conf.yml"))
  expect_equal(
    names(conf),
    c("single_mem","pool_mem","dap","cds")
  )
  expect_true("Driver" %in% names(conf$dap$connection))
})

test_that("reconnections are live", {
  fn <- testthat::test_path("testfiles","sqlhelper_db_conf.yml")
  reconnect(.fn = fn,
            exclusive = TRUE)
  expect_true(is.connected('single_mem'))
  expect_true(is.connected('pool_mem'))
})

test_that("connections_list returns a list of live connections", {
  expect_equal(connections_list(), c("single_mem","pool_mem"))
  close_connections()
  expect_equal(connections_list(),as.character(c()))
  set_connections(config_filename = test_path("testfiles","sqlhelper_db_conf.yml"),
                  exclusive = TRUE)
  expect_equal(connections_list(), c("single_mem","pool_mem"))
})

test_that("live_connection returns the named connection or null",{

  expect_equal(is(live_connection("pool_mem")),
               c("Pool")
  )
  reconnect(.fn = test_path("testfiles","sqlhelper_db_conf.yml"))
  expect_equal(is(live_connection("single_mem")),
               c("SQLiteConnection","DBIConnection","DBIObject")
  )
  #expect_warning(out <- live_connection("bar"), regexp = "No connection named")
  out <- suppressWarnings(live_connection("bar"))
  expect_null(out)

})
