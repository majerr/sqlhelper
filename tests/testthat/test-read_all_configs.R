test_that("parameters produce correct results", {
  skip_on_cran()
  expect_equal(names(read_all_configs()),
               c('dap','cds'))
  expect_equal(names(read_all_configs(testthat::test_path("testfiles",
                                                          "sqlhelper_db_conf.yml"))),
               c('single_mem',
                 'pool_mem',
                 'dap',
                 'cds')
  )
  expect_equal(names(read_all_configs(testthat::test_path("testfiles",
                                                          "sqlhelper_db_conf.yml"),
                                      exclusive = TRUE)),
               c('single_mem',
                 'pool_mem')
  )


})

test_that("appropriate warnings are issued", {
  skip_on_cran()

  expect_warning(read_all_configs("site"),
                 "No file named \'sqlhelper_db_conf.yml\' was found in"
  )
  expect_warning(read_all_configs("foo"),
                 "foo is not a filename"
  )
})
