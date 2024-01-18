test_that("runqueries runs queries in sequence", {
  connect( testthat::test_path("testfiles",
                               "sqlhelper_db_conf.yml"),
           exclusive = TRUE)


  DBI::dbWriteTable(default_conn(),
                    DBI::SQL("iris"),
                    iris)

  queries <- read_sql(
    testthat::test_path("testfiles",
                        "sequential_queries.sql")
    )

  runqueries( queries )

  expect_identical(DBI::dbGetQuery(default_conn(),
                               "SELECT COUNT(*) FROM IRIS3")[[1,1]],
                   nrow(head(iris,10)))
})

test_that("runfiles runs files", {
  results <- runfiles(
    c(
      testthat::test_path("testfiles",
                           "test_runfiles.sql"),
      testthat::test_path("testfiles",
                          "test_runfiles2.sql")
      )
  )
  iris_char <- iris
  iris_char$Species <- as.character(iris$Species)
  expect_identical(results$two,
                   head(iris_char, 50))

  expect_identical(results$three,
                   head(iris_char, 10))
})

# test quoting
test_that("quotesql works",{
  species <- "'setosa'; DROP TABLE IRIS;"
  result <- runqueries(c(injection="SELECT * FROM IRIS WHERE species LIKE {species}",
                         survived="SELECT * FROM SQLITE_SCHEMA WHERE type='table' AND name='iris'"))
  expect_equal(nrow(result$injection), 0)
  expect_equal(nrow(result$survived), 1)
  expect_warning(runqueries("SELECT * FROM IRIS WHERE species LIKE {species}",
                       quotesql = "no"),
                 "Ignoring remaining part of query") # RSQLite recognizes and refuses multiple queries in a string

})

# test execmethod & geometry

## Spatial read (geometry)
test_that("spatial read works", {

  sf::st_write(spData::congruent, default_conn(), "congruent")

  spatial_read <- runqueries(c("congruent"="SELECT * FROM congruent"),
                        default.conn = default_conn(),
                        execmethod = "spatial",
                        geometry = "geometry")

  # My first attempt was to test with:

  # expect_equal(results$congruent, spData::congruent)

  # but this fails - the CRS string changes during the write/read operation. Issue raised
  # with sf maintainers: https://github.com/r-spatial_read/sf/issues/2017
  # ... and closed without explanation. Must have been a stupid question.

  # Turns out CRS strings are not necessarily expected to be equal:
  # https://github.com/r-spatial_read/sf/issues/180

  expect_equal(names(spatial_read$congruent), names(spData::congruent))
  expect_equal(spatial_read$congruent[[1]],
               spData::congruent[[1]])
  expect_equal(spatial_read$congruent[[2]],
               spData::congruent[[2]])

  expect_true(sf::st_crs(spatial_read$congruent$geometry) ==
                sf::st_crs(spData::congruent$geometry))

  expect_s3_class(spatial_read$congruent$geometry, "sfc")

  getq <- runqueries(c("congruent"="SELECT * FROM congruent"),
                     default.conn = live_connection("single_mem"))

  expect_type(getq$congruent$geometry, "character")
})

## send-bind-fetch

test_that("send-bind-fetch works", {
  # Send-bind-fetch doesn't work with pool connections:
  # http://rstudio.github.io/pool/reference/DBI-custom.html

    petalwidth_filter <- runqueries("select * from iris where `Petal.Width` < ?",
                       execmethod = "sends")

    limits <- list( upper = 2.3, lower = .2 )

    DBI::dbBind( petalwidth_filter, limits$lower )
    lower_fetched <- DBI::dbFetch( petalwidth_filter ) |>
      tibble::remove_rownames()

    lower_df <- iris[iris$Petal.Width < limits$lower,] |>
      dplyr::mutate_if(is.factor,as.character) |>
      tibble::remove_rownames()

    DBI::dbBind( petalwidth_filter, limits$upper )
    upper_fetched <- DBI::dbFetch( petalwidth_filter ) |>
      tibble::remove_rownames()

    upper_df <- iris[iris$Petal.Width < limits$upper,] |>
      dplyr::mutate_if(is.factor,as.character) |>
      tibble::remove_rownames()

    expect_identical(lower_fetched, lower_df)
    expect_identical(upper_fetched, upper_df)

    DBI::dbClearResult( petalwidth_filter )

})

# test connection
test_that("correct connections are used", {
  runqueries("DROP TABLE iris",execmethod="execute")

  iris_char <- iris
  iris_char$Species <- as.character(iris$Species)

  DBI::dbWriteTable(live_connection("pool_mem"),"iris",iris)
  DBI::dbWriteTable(live_connection("single_mem"),"arrests",USArrests)
  DBI::dbWriteTable(live_connection("single_mem2"),"cars",mtcars)

  # default connection
  expect_error(runqueries("select * from iris"))
  expect_identical(runqueries("select * from arrests") |> tibble::remove_rownames()
                   ,USArrests |> tibble::remove_rownames())

  # default connection in files
  # interpreted comment connection
  results <- runfiles(
      testthat::test_path("testfiles",
                          "test_runfiles_connection.sql")
  )



  expect_identical(results[[1]] |> tibble::remove_rownames()
                   ,USArrests |> tibble::remove_rownames())
  expect_identical(results[[2]] |> tibble::remove_rownames()
                   ,iris_char |> tibble::remove_rownames())
  expect_identical(results[[3]] |> tibble::remove_rownames(),
                   mtcars |> tibble::remove_rownames())

  expect_error(
    runfiles(
      testthat::test_path("testfiles",
                          "test_runfiles_connection_fail.sql")
    ),
    "no such table"
  )

  # supplied default connection

  expect_identical(runqueries("select * from cars", default.conn = "single_mem2") |>
                     tibble::remove_rownames(),
                   mtcars |> tibble::remove_rownames())

  expect_identical(runfiles(testthat::test_path("testfiles",
                                                "test_runfiles_new_default_connection.SQL"),
                            default.conn = "single_mem2") |>
                     tibble::remove_rownames(),
                   mtcars |> tibble::remove_rownames())

  # cascaded interpreted comment connection
  result <- runfiles(testthat::test_path("testfiles",
                                         "test_runfiles_cascade_connection.SQL"))

  expect_identical(result[[2]] |> tibble::remove_rownames(),
                   mtcars[mtcars$mpg > 22.80,] |> tibble::remove_rownames())

  # cascade turned off
  expect_error(
    runfiles(
      testthat::test_path("testfiles",
                          "test_runfiles_cascade_connection.SQL"),
      cascade = FALSE),
    "no such table"
    )

  # cascade should be done per-file
  result <- runfiles(
    c(testthat::test_path("testfiles",
                          "test_runfiles_cascade_connection.SQL"),
      testthat::test_path("testfiles",
                          "test_runfiles_cascade_connection2.SQL")))

  expect_identical(result[[2]] |> tibble::remove_rownames(),
                   mtcars[mtcars$mpg > 22.80,] |> tibble::remove_rownames())
  expect_identical(result[[3]] |> tibble::remove_rownames(),
                   USArrests |> tibble::remove_rownames())

  expect_error(
    runfiles(
      c( testthat::test_path("testfiles",
                             "test_runfiles_cascade_connection.SQL"),
         testthat::test_path("testfiles",
                             "test_runfiles_cascade_connection2.SQL") ),
      default.conn = "single_mem2"),
    "no such table"
  )

})

# test interpolate
test_that("interpolation can be turned off", {

  n <- 5
  expect_equal(runqueries("select {n} as 'n'"), data.frame(n))
  expect_error(runqueries("select {n} as 'n'", values=NULL),
               "unrecognized token")


  expect_equal(
    runfiles( testthat::test_path( "testfiles", "test_runfiles_interpolate.SQL" ) ),
      data.frame("n" = n+1)
  )

  expect_error(
    runfiles( testthat::test_path( "testfiles", "test_runfiles_interpolate.SQL" ),
              values = NULL),
    "unrecognized token"
  )

  expect_equal(
    runfiles( testthat::test_path( "testfiles", "test_runfiles_interpolate.SQL" ) ),
    data.frame("n" = n+1)
  )

  expect_error(
    runfiles( testthat::test_path( "testfiles", "test_runfiles_no_interpolate.SQL" ),
              values = NULL),
    "unrecognized token"
  )

})

# test include_params
test_that("parameters can be included when required", {

  expect_equal(runqueries("select 1 as 'n'", include_params = TRUE),
               tibble::tibble(qname = as.character(NA),
                              quotesql = "yes",
                              interpolate = "yes",
                              execmethod = "get",
                              geometry = as.character(NA),
                              conn_name = "default",
                              sql = "select 1 as 'n'",
                              filename = as.character(NA),
                              prepared_sql = list(DBI::SQL("select 1 as 'n'")),
                              result = list(data.frame(n=1)))
  )

  expect_equal(
    runfiles(
      testthat::test_path( "testfiles", "test_runfiles_include_params.SQL" ),
      include_params = TRUE),
    tibble::tibble(qname = as.character(NA),
                   quotesql = "yes",
                   interpolate = "yes",
                   execmethod = "get",
                   geometry = as.character(NA),
                   conn_name = "default",
                   sql = "select 1 as 'n'",
                   filename = c("test_runfiles_include_params"),
                   prepared_sql = list(DBI::SQL("select 1 as 'n'")),
                   result = list(data.frame(n=1)))
  )


})

