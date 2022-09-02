test_that("parameters produce correct results", {
  skip_on_cran()
  expect_equal(names(read_configs()),
               c('dap','cds'))
  expect_equal(names(read_configs(testthat::test_path("testfiles",
                                                          "sqlhelper_db_conf.yml"))),
               c('single_mem',
                 'pool_mem',
                 'dap',
                 'cds')
  )
  expect_equal(names(read_configs(testthat::test_path("testfiles",
                                                          "sqlhelper_db_conf.yml"),
                                      exclusive = TRUE)),
               c('single_mem',
                 'pool_mem')
  )


})

test_that("appropriate warnings are issued", {
  skip_on_cran()
  expect_warning(read_configs("foo"),
                 "Configuration file 'foo' does not exist"
  )
})

test_that("appropriate errors are raised",{
  skip_on_cran()

  expect_error(read_configs(exclusive=TRUE),
               "A configuration filename is required")

  expect_error(read_configs("site",exclusive=TRUE),
               "Configuration file .* does not exist")

  expect_error(read_configs("foo",exclusive=TRUE),
               "Configuration file foo does not exist")


})
