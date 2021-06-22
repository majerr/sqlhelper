context("connections")
library(sqlhelper)

test_that("connections are live", {
  expect_true(is.connected('cds'))
})

test_that("reconnections are live", {
  reconnect()
  expect_true(is.connected('cds'))
})

test_that("connections_list returns a list of live connections", {
  expect_equal(connections_list(), c("cds"))
  close_connections()
  expect_equal(connections_list(),as.character(c()))
  set_connections()
})

test_that("live_connection returns the named connection or null",{
  expect_equal(live_connection("cds"), connections$cds)
  expect_null(live_connection("foo"))
})
