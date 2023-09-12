test_that("parameters produce correct results", {
  expect_equal(names(read_configs(testthat::test_path("testfiles",
                                                          "sqlhelper_db_conf.yml"),
                                  exclusive = TRUE)),
               c('single_mem',
                 'single_mem2',
                 'pool_mem')
  )
  expect_equal(names(read_configs(testthat::test_path("testfiles",
                                                          "sqlhelper_db_conf2.yml"),
                                      exclusive = TRUE)),
               c('dap',
                 'cds')
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

  site_fn <- file.path(
    rappdirs::site_config_dir(),
    conf_fn
  )

  expect_error(read_configs("site",exclusive=TRUE),
               "Configuration file .* does not exist")

  expect_error(read_configs("foo",exclusive=TRUE),
               "Configuration file foo does not exist")
})
