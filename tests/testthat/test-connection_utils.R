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
