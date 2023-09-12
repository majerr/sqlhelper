test_that("is_connected returns NULL if no cache is found", {
  disconnect()
  expect_null(is_connected())
})

test_that("is_connected returns TRUE if the named connection is live, else FALSE", {
  connect(config_filename = testthat::test_path("testfiles",
                                                "sqlhelper_db_conf.yml"),
          exclusive=TRUE)
  expect_true(is_connected("single_mem"))
  expect_null(is_connected("foo"))
  DBI::dbDisconnect(live_connection("single_mem"))
  expect_false(is_connected("single_mem"))
  disconnect()
})
