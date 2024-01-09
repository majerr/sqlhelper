
test_that("is_connected returns an appropriate value", {
  connect(config_filename = testthat::test_path("testfiles",
                                                "sqlhelper_db_conf.yml"),
          exclusive=TRUE)
  expect_true(is_connected("single_mem"))

  DBI::dbDisconnect(
    live_connection("single_mem")
  )
  expect_false(is_connected("single_mem"))
  expect_null(is_connected("foo"))

  disconnect()
  expect_null(is_connected("single_mem"))
  expect_null(is_connected("foo"))
})

test_that("not_connected returns an appropriate value", {
  connect(config_filename = testthat::test_path("testfiles",
                                                "sqlhelper_db_conf.yml"),
          exclusive=TRUE)
  expect_false(not_connected("single_mem"))
  DBI::dbDisconnect(
    live_connection("single_mem")
  )
  expect_true(not_connected("single_mem"))
  expect_null(not_connected("foo"))

  disconnect()
  expect_null(is_connected("single_mem"))
  expect_null(is_connected("foo"))

  expect_null(not_connected("single_mem"))
  expect_null(not_connected("foo"))
})
