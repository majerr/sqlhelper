library(sqlhelper)

test_that("get_config reads appropriate config file", {

  expect_equal(
    names(
      read_config_file(
        testthat::test_path("testfiles",
                            "sqlhelper_db_conf.yml")
      )
    ),
    c("mem")
  )

})

test_that("get_all_configs appropriately combines config files", {
  skip_on_cran()
  conf <- get_all_configs(testthat::test_path("testfiles",
                                              "sqlhelper_db_conf.yml"))
  expect_equal(
    names(conf),
    c("mem","dap","cds")
  )
  expect_true("Driver" %in% names(conf$dap$connection))
})

test_that("reconnections are live", {
  reconnect(.fn = test_path("testfiles","sqlhelper_db_conf.yml"),
            exclusive = TRUE)
  expect_true(is.connected('mem'))
})

test_that("connections_list returns a list of live connections", {
  expect_equal(connections_list(), c("mem"))
  close_connections()
  expect_equal(connections_list(),as.character(c()))
  set_connections(config_filename = test_path("testfiles","sqlhelper_db_conf.yml"),
                  exclusive = TRUE)
  expect_equal(connections_list(), c("mem"))
  close_connections()
  expect_equal(connections_list(),as.character(c()))
  set_connections(config_filename = test_path("testfiles","sqlhelper_db_conf.yml"))
  expect_equal(connections_list(), c("mem","dap","cds"))
})

test_that("live_connection returns the named connection or null",{

  expect_equal(is(live_connection("mem")),
               c("SQLiteConnection","DBIConnection","DBIObject")
  )
  expect_warning(
    expect_null(live_connection("bar"))
  )
})
