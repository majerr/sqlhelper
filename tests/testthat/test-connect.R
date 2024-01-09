

test_that("connections can be added and discovered",{
  expect_null(connection_info())

  connect(config_filename = testthat::test_path("testfiles",
                                                "sqlhelper_db_conf.yml"),
          exclusive=TRUE)


  expect_equal(connection_info()$name,
               c("single_mem2","single_mem","pool_mem"))

  add_connection("sqlite_dbi",list("driver_type"="sqlite",
                                   "connection"=c("Server"=":memory:")))

  expect_true("sqlite_dbi" %in% connection_info()$name)
})

test_that("live_connection returns invisible nulls as expected",{
  expect_null(
    live_connection("foo")
  )

  DBI::dbDisconnect(live_connection("sqlite_dbi"))

  expect_null(
    live_connection("sqlite_dbi")
  )
})

test_that("default_conn returns the expected connection", {

  defcon1 <- default_conn()
  defcon2 <- get_default_conn_name() |> live_connection()

  expect_equal(defcon1, defcon2)
})

test_that("pruning an unnamed connection raises the right error",{
  expect_error(
    sqlhelper:::prune("foo"),
    "No connection named foo"
  )
})

test_that("connections can be closed",{
  expect_equal(connection_info()$name,
               c("single_mem2","single_mem","pool_mem","sqlite_dbi"))
  disconnect()
  expect_null(connection_info())
})

test_that("messages and warnings are raised on failure", {
  expect_message(
    suppressWarnings(
      connect(config_filename = testthat::test_path("testfiles",
                                                    "sqlhelper_db_conf_fail.yml"),
              exclusive=TRUE)
    ),
    "Error whilst connecting sqlserver_fail"
    )

  expect_warning(
    suppressMessages(
      connect(config_filename = testthat::test_path("testfiles",
                                                    "sqlhelper_db_conf_fail.yml"),
              exclusive=TRUE)
    ),
    "sqlserver_fail is not available"
  )
})

