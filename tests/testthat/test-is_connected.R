test_that("is_connected returns FALSE if no cache is found", {
  disconnect()
  expect_false(is_connected("single_mem"))
})

test_that("is_connected returns TRUE if the named connection is live, else FALSE", {
  connect(config_filename = testthat::test_path("testfiles",
                                                "sqlhelper_db_conf.yml"),
          exclusive=TRUE)
  expect_true(is_connected("single_mem"))
  expect_false(is_connected("foo"))
  DBI::dbDisconnect(live_connection("single_mem"))
  expect_false(is_connected("single_mem"))
  disconnect()
})

test_that("not_connected returns the opposite of is_connected", {
  disconnect()
  expect_equal(not_connected("foo"), !is_connected("foo"))

  connect(config_filename = testthat::test_path("testfiles",
                                                "sqlhelper_db_conf.yml"),
          exclusive=TRUE)

  expect_equal(not_connected("foo"), !is_connected("foo"))
  expect_equal(not_connected("single_mem"), !is_connected("single_mem"))
  disconnect()
})
