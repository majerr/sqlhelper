test_that("yml2conn_str returns an appropriate string", {

  fn <- testthat::test_path("testfiles",
                            "yml2conn_str_test.yml")

  configs <- validate_configs(
    read_configs(fn,exclusive=TRUE)
  )

  expect_equal(yml2conn_str(configs$dap$connection),
               "Driver={Odbc Driver 17 For Sql Server}; Server=Dap-Sql01; Trusted_Connection=Yes")

  expect_equal(yml2conn_str(configs$cds$connection),
               "Driver={Odbc Driver 17 For Sql Server}; Server=Dap-Sql01\\Cds; Trusted_Connection=Yes")
})

test_that("connections can be added and discovered",{
  expect_null(connection_info())
  connect(fn = testthat::test_path("testfiles",
                                    "sqlhelper_db_conf.yml"),
          exclusive=TRUE)
  expect_equal(connection_info()$name,
               c("single_mem","pool_mem"))
  add_connection("sqlite_dbi",list("server_type"="sqlite",
                                   "Server"=":memory:"))
  expect_true("sqlite_dbi" %in% connection_info()$name)
})

test_that("connections can be closed",{
  expect_equal(connection_info()$name,
               c("single_mem","pool_mem","sqlite_dbi"))
  close_connections()
  expect_null(connection_info())
})



