test_that("Driver returns the appropriate function", {

  fn <- testthat::test_path("testfiles",
                            "driver_test.yml")

  configs <- validate_configs(
    read_configs(fn,exclusive=TRUE)
  )

  expect_equal(driver(configs$default_missing), odbc::odbc)
  expect_equal(driver(configs$default_unrecognized), odbc::odbc)
  expect_equal(driver(configs$sqlite), RSQLite::SQLite)
  expect_equal(driver(configs$postgresql), RPostgres::Postgres)
  expect_equal(driver(configs$mysql), RMariaDB::MariaDB)
  expect_equal(driver(configs$mariadb), RMariaDB::MariaDB)
  expect_equal(driver(configs$bigquery), bigrquery::bigquery)
  expect_equal(driver(configs$odbc), odbc::odbc)

})

test_that("yml2conn_str returns an appropriate string", {

  fn <- testthat::test_path("testfiles",
                            "yml2conn_str_test.yml")

  configs <- validate_configs(
    read_configs(fn,exclusive=TRUE)
  )

  expect_equal(yml2conn_str(configs$dap),
               "Driver={Odbc Driver 17 For Sql Server}; Server=Dap-Sql01; Trusted_Connection=Yes")

  expect_equal(yml2conn_str(configs$cds),
               "Driver={Odbc Driver 17 For Sql Server}; Server=Dap-Sql01\\Cds; Trusted_Connection=Yes")
})
