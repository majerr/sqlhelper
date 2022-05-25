context("connections")
library(sqlhelper)

test_that("get_config reads appropriate config files", {
  # Site wide
  expect_true(is.na(read_config_file("site")))

  # User conf
  expect_equal(names(read_config_file("user")),c("cds","dap"))

  # Named file
  expect_equal(names(read_config_file("conf/sqlhelper_db_conf.yml")), c("cds","dap","foo"))

  # Default name in current dir
  expect_equal(names(read_config_file()), c("dap", "foo"))
})

test_that("get_all_configs appropriately combines config files", {
  conf <- get_all_configs()
  expect_true("Driver" %in% names(conf$dap))
})

test_that("connections are live", {
  expect_true(is.connected('cds'))
})

test_that("reconnections are live", {
  reconnect()
  expect_true(is.connected('cds'))
})

test_that("connections_list returns a list of live connections", {
  expect_equal(connections_list(), c("cds", "dap", "foo"))
  close_connections()
  expect_equal(connections_list(),as.character(c()))
  set_connections()
  expect_equal(connections_list(), c("cds", "dap", "foo"))
})

test_that("live_connection returns the named connection or null",{
  expect_equal(live_connection("cds"), connections$cds)
  expect_warning(
    expect_null(live_connection("bar"))
  )
})
