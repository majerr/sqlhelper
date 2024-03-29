# Check names of lists are properly transferred
test_that("names are preserved", {
  connect(config_filename = testthat::test_path("testfiles",
                                                "sqlhelper_db_conf.yml"),
          exclusive=TRUE)

  sql <- prepare_sql(c(one="SELECT 1", two="SELECT2"))
  expect_equal(sql$qname, c("one", "two"))
})

# Check errors are raised for improperly formed inputs (wrong cols, wrong rows)
test_that("inputs are valid", {
  expect_error(prepare_sql(tibble::tibble(a=c(1,2),b=c(3,4))), "must have these columns")
  expect_error(prepare_sql(list(one="SELECT 1", two="SELECT2")), "must be a character vector or a tibble")
  expect_error(prepare_sql(c(one=1, two=2)), "must be a character vector or a tibble")

  expect_error(
    prepare_sql(
      tibble::tibble(qname=NA,
                     quotesql=NA,
                     interpolate=NA,
                     execmethod=NA,
                     geometry=NA,
                     conn_name=NA,
                     sql=c(NA,"foo"),
                     filename=NA)
    ),
    "sql argument contains NAs"
  )

  expect_error(
    prepare_sql(
      tibble::tibble(qname=NA,
                     quotesql=NA,
                     interpolate=NA,
                     execmethod=NA,
                     geometry=NA,
                     conn_name=NA,
                     sql=NA,
                     filename=NA)
    ),
    "sql argument contains NAs"
  )
})

# Check that only NAs are replaced with defaults (and that they are properly replaced)
test_that("NAs are replaced by defaults", {
  n <- 5
  sql <- read_sql( testthat::test_path( "testfiles", "test_prepare.sql")) |>
    prepare_sql()
  expect_equal(sql$conn_name, c('default', 'default', 'foo'))
  expect_equal(sql$quotesql, c("yes","no", "no"))
  expect_equal(sql$execmethod, c("get","execute", "spatial"))
  expect_equal(sql$geometry, c(NA, NA, "geom"))
  expect_equal(sql$prepared_sql,
               list(
                 DBI::SQL("SELECT 5"),
                 DBI::SQL("SELECT 1"),
                 DBI::SQL("select 'foo'")
                 )
               )


  sql <- read_sql( testthat::test_path( "testfiles", "test_prepare.sql")) |>
    prepare_sql(default.conn = "pool_mem", quotesql = "no", execmethod = "sendq", geometry="g")
  expect_equal(sql$conn_name, c('default', 'default', 'foo'))
  expect_equal(sql$quotesql, c("no","no", "no"))
  expect_equal(sql$execmethod, c("sendq","execute", "spatial"))
  expect_equal(sql$geometry, c("g", "g", "geom"))

})

# Check interpolations


test_that("defaults are properly set", {
  connect(config_filename = testthat::test_path("testfiles",
                                                "sqlhelper_db_conf.yml"),
          exclusive=TRUE)

  # test default defaults
  sql <- prepare_sql(c(one="select 1"))

  expect_equal(sql$qname, "one")
  expect_equal(sql$quotesql, "yes")
  expect_equal(sql$interpolate, "yes")
  expect_equal(sql$execmethod, "get")
  expect_true(is.na(sql$geometry))
  expect_equal(sql$conn_name, "default")
  expect_equal(sql$sql, "select 1")
  expect_true(is.na(sql$filename))
  expect_equal(sql$prepared_sql[[1]], DBI::SQL("select 1"))

  # change defaults

})

# interpolation is correct
test_that("interpolation is correct", {

  # value not available
  expect_error(prepare_sql(c(one="select {x}")))

  # interpolate settings are obeyed: interpolate
  sql <- prepare_sql(c(one="select {x}"), values = NULL)
  expect_equal(sql$interpolate, "no")
  expect_false(sql$prepared_sql[[1]] == "select 3")

  x = 3
  sql <- prepare_sql(c(one="select {x}"))
  expect_true(sql$prepared_sql[[1]] == "select 3")

  # parent.env vs alt env
  env <- new.env()
  env$n <- 2
  env$foo <- "bar"
  sql <- prepare_sql(c(one="select {n}", twi="select {foo}"), values = env)
  expect_equal(sql$prepared_sql,list(DBI::SQL("select 2"),DBI::SQL("select 'bar'")))

  # settings are obeyed: quotesql
  sql <- prepare_sql(c(one="select {foo}"), values = env, quotesql = "no")
  expect_equal(sql$prepared_sql,list(DBI::SQL("select bar")))

})
