test_that("get_driver recognizes db types",{
  expect_error(get_driver("foo","bar"))

  # Can't think how to test both sides of this without interferring with other
  # people's envs. These pass because I have the libs installed, but ideally want
  # to test failure when they are not, too.
  driver <- get_driver("testcon", "odbc")
  expect_equal(driver$driver_name, "odbc::odbc")
  expect_identical(driver$driver_func, odbc::odbc)

  driver <- get_driver("testcon", "sqlite")
  expect_equal(driver$driver_name, "RSQLite::SQLite")
  expect_identical(driver$driver_func, RSQLite::SQLite)

  driver <- get_driver("testcon", "postgresql")
  expect_equal(driver$driver_name, "RPostgres::Postgres")
  expect_identical(driver$driver_func, RPostgres::Postgres)

  driver <- get_driver("testcon", "mysql")
  expect_equal(driver$driver_name, "RMariaDB::MariaDB")
  expect_identical(driver$driver_func, RMariaDB::MariaDB)

  driver <- get_driver("testcon", "mariadb")
  expect_equal(driver$driver_name, "RMariaDB::MariaDB")
  expect_identical(driver$driver_func, RMariaDB::MariaDB)

  driver <- get_driver("testcon", "bigquery")
  expect_equal(driver$driver_name, "bigrquery::bigquery")
  expect_identical(driver$driver_func, bigrquery::bigquery)

})
