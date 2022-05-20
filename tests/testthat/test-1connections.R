context("connections")
library(sqlhelper)

test_that("get_config reads appropriate config files", {
  expect_true(is.na(read_config_file("site")))
  expect_equal(names(read_config_file("user")),c("cds","dap"))
  expect_equal(names(read_config_file(".sqlhelper_db_test_conf.yml")), c("cds","dap"))
  expect_equal(names(read_config_file()), c("cds","dap"))
})

test_that("get_all_configs appropriately combines config files", {
  conf <- get_all_configs()
  expect_true("connection" %in% names(conf$dap))
  expect_equal(names(conf$dap$connection),names(conf$dap[2:length(conf$dap)]))
})

test_that("connections are live", {
  expect_true(is.connected('cds'))
})

test_that("reconnections are live", {
  reconnect()
  expect_true(is.connected('cds'))
})

test_that("connections_list returns a list of live connections", {
  expect_equal(connections_list(), c("cds", "dap"))
  close_connections()
  expect_equal(connections_list(),as.character(c()))
  set_connections()
  expect_equal(connections_list(), c("cds", "dap"))
})

test_that("live_connection returns the named connection or null",{
  expect_equal(live_connection("cds"), connections$cds)
  expect_warning(
    expect_null(live_connection("foo"))
  )
})
