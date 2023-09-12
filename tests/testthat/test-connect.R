

test_that("connections can be added and discovered",{
  expect_null(connection_info())

  connect(config_filename = testthat::test_path("testfiles",
                                                "sqlhelper_db_conf.yml"),
          exclusive=TRUE)


  expect_equal(connection_info()$name,
               c("single_mem","pool_mem"))

  add_connection("sqlite_dbi",list("driver_type"="sqlite",
                                   "connection"=c("Server"=":memory:")))

  expect_true("sqlite_dbi" %in% connection_info()$name)
})

test_that("connections can be closed",{
  expect_equal(connection_info()$name,
               c("single_mem","pool_mem","sqlite_dbi"))
  disconnect()
  expect_null(connection_info())
})



