context("connections")
library(sqlhelper)

test_that("connections are live", {
  expect_true(is.connected('cds'))
})

test_that("reconnections are live", {
  reconnect()
  expect_true(is.connected('cds'))
})
