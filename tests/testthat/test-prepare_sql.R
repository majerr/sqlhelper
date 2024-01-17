# Check names of lists are properly transferred
test_that("names are preserved", {
  connect(config_filename = testthat::test_path("testfiles",
                                                "sqlhelper_db_conf.yml"),
          exclusive=TRUE)

  sql <- prepare_sql(c(one="SELECT 1", two="SELECT2"))
  expect_equal(sql$qname, c("one", "two"))
})

# Check errors are raised for improperly formed inputs (wrong cols, wrong rows)
test_that("inputs are validated", {
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

  expect_identical(
    prepare_sql(
      tibble::tibble(qname=NA,
                     quotesql=NA,
                     interpolate=NA,
                     execmethod=NA,
                     geometry=NA,
                     conn_name=NA,
                     sql=c("SELECT 'foo'"),
                     filename=NA)
    ),
    tibble::tibble(qname=as.character(NA),
                   quotesql="yes",
                   interpolate="yes",
                   execmethod="get",
                   geometry=as.character(NA),
                   conn_name="default",
                   sql=c("SELECT 'foo'"),
                   filename=as.character(NA),
                   prepared_sql=list(DBI::SQL("SELECT 'foo'")))
  )

  expect_error(prepare_sql("select 1", default.conn = "foo"),
               "is not a valid connection")

  bust <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  DBI::dbDisconnect(bust)
  expect_error(prepare_sql("select 1", default.conn = bust),
               "is not a valid connection")

  foo <- "bar"
  expect_equal(prepare_sql("select {`foo`}")$prepared_sql[[1]],
               DBI::SQL("select `bar`"))
  DBI::dbDisconnect(live_connection("single_mem"))
  expect_error(prepare_sql("select {`foo`}"),
               "is not a valid connection")
  expect_error(prepare_sql("select {`foo`}",default.conn = "single_mem"),
               "is not a valid connection")

  connect(config_filename = testthat::test_path("testfiles",
                                                "sqlhelper_db_conf.yml"),
          exclusive=TRUE)
})

# Check that only NAs are replaced with defaults (and that they are properly replaced)
test_that("NAs are replaced by defaults", {
  n <- 5
  foo <- "bar"
  sql <- read_sql( testthat::test_path( "testfiles", "test_prepare.SQL")) |>
    prepare_sql()
  expect_equal(sql$conn_name, c('default', 'default', 'default', 'default', 'pool_mem'))
  expect_equal(sql$quotesql, c("yes", "yes", "yes", "no", "no"))
  expect_equal(sql$execmethod, c("get", "get", "get", "execute", "spatial"))
  expect_equal(sql$geometry, c(NA, NA, NA, NA, "geom"))
  expect_equal(sql$prepared_sql,
               list(
                 DBI::SQL("SELECT 5"),
                 DBI::SQL("SELECT `bar`"),
                 DBI::SQL("SELECT {`foo`}"),
                 DBI::SQL("SELECT bar"),
                 DBI::SQL("select 'foo'")
                 )
               )


  sql <- read_sql( testthat::test_path( "testfiles", "test_prepare.SQL")) |>
    prepare_sql(default.conn = "pool_mem", quotesql = "no", execmethod = "sendq", geometry="g")
  expect_equal(sql$conn_name, c('default', 'default', 'default', 'default', 'pool_mem'))
  expect_equal(sql$quotesql, c("no", "no","no", "no", "no"))
  expect_equal(sql$execmethod, c("sendq", "sendq","sendq", "execute", "spatial"))
  expect_equal(sql$geometry, c("g", "g", "g", "g", "geom"))

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
  sql <- prepare_sql(c(one="select 1"),
                     quotesql = "no",
                     values = "no",
                     execmethod = "sendq",
                     default.conn = "pool_mem"
                     )

  expect_equal(sql$quotesql, "no")
  expect_equal(sql$interpolate, "no")
  expect_equal(sql$execmethod, "sendq")
  #expect_equal(sql$conn_name, "pool_mem")

  expect_error(prepare_sql("SELECT {`foo`}"))
  expect_error(prepare_sql("SELECT {`foo`}", quotesql = "no"))
})

test_that("correct connections are used for interpolation",{
  skip("Requires local SQL Server instance") # comment if local
  foo <- "noo"
  bar <- "baz"
  sql <- "select {`bar`} from {`foo`}"
  expect_equal(
    sqlhelper:::interpolate_sql(
      interpolate = "yes",
      quotesql = "yes",
      conn_name = "default",
      sql = sql,
      values = environment(),
      default.conn = default_conn()),

    DBI::SQL("select `baz` from `noo`")
  )

  # Local sqlserver install
  con <- DBI::dbConnect(odbc::odbc(),
                        Driver = "{ODBC Driver 17 for SQL Server}",
                        Server = "Dap-sql01\\cds",
                        Trusted_Connection = "yes")

  expect_equal(
    sqlhelper:::interpolate_sql(
      interpolate = "yes",
      quotesql = "yes",
      conn_name = "default",
      sql = sql,
      values = environment(),
      default.conn = con),

    DBI::SQL("select \"baz\" from \"noo\"")
  )
})

# interpolation is correct
test_that("interpolation is correct", {

  # connection specified in comment is bad
  expect_error(
    read_sql(
      testthat::test_path( "testfiles", "test_prepare.SQL")
    ) |>
      prepare_sql()
  )

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
  n <- 5
  foo <- "fye"
  env <- new.env()
  env$n <- 2
  env$foo <- "bar"
  sql <- prepare_sql(c(one="select {n}", two="select {foo}"), values = env)
  expect_equal(sql$prepared_sql,list(DBI::SQL("select 2"),DBI::SQL("select 'bar'")))

  # settings are obeyed: quotesql
  sql <- prepare_sql(c(one="select {foo}"), values = env, quotesql = "no")
  expect_equal(sql$prepared_sql,list(DBI::SQL("select bar")))

})
