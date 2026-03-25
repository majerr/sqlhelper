test_that("get_driver recognizes db types",{
  expect_error(get_driver("foo","bar"))
})

test_that("get_driver works for odbc", {
  skip_if_not_installed("odbc")
  driver <- get_driver("testcon", "odbc")
  expect_equal(driver$driver_name, "odbc::odbc")
  expect_identical(driver$driver_func, odbc::odbc)
})

test_that("get_driver works for sqlite", {
  skip_if_not_installed("RSQLite")
  driver <- get_driver("testcon", "sqlite")
  expect_equal(driver$driver_name, "RSQLite::SQLite")
  expect_identical(driver$driver_func, RSQLite::SQLite)
})

test_that("get_driver works for postgresql", {
  skip_if_not_installed("RPostgres")
  driver <- get_driver("testcon", "postgresql")
  expect_equal(driver$driver_name, "RPostgres::Postgres")
  expect_identical(driver$driver_func, RPostgres::Postgres)
})

test_that("get_driver works for mysql", {
  skip_if_not_installed("RMariaDB")
  driver <- get_driver("testcon", "mysql")
  expect_equal(driver$driver_name, "RMariaDB::MariaDB")
  expect_identical(driver$driver_func, RMariaDB::MariaDB)
})

test_that("get_driver works for mariadb", {
  skip_if_not_installed("RMariaDB")
  driver <- get_driver("testcon", "mariadb")
  expect_equal(driver$driver_name, "RMariaDB::MariaDB")
  expect_identical(driver$driver_func, RMariaDB::MariaDB)
})

test_that("get_driver works for bigquery", {
  skip_if_not_installed("bigrquery")
  driver <- get_driver("testcon", "bigquery")
  expect_equal(driver$driver_name, "bigrquery::bigquery")
  expect_identical(driver$driver_func, bigrquery::bigquery)
})
