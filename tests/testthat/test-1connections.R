context("connections")
library(ucsqlhelper)

test_that("connections are live", {
  expect_true(RODBC:::odbcValidChannel(connections$hive))
  expect_true(RPostgreSQL::isPostgresqlIdCurrent(connections$pg))
})

test_that("reconnections are live", {
  reconnect()
  expect_true(RODBC:::odbcValidChannel(connections$hive))
  expect_true(RPostgreSQL::isPostgresqlIdCurrent(connections$pg))
})
