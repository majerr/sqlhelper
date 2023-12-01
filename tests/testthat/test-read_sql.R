test_that("The correct number of queries are read", {
  sql <- read_sql(testthat::test_path("testfiles",
                                      "test_read.SQL"))
  expect_equal(nrow(sql),4)
})

test_that("Comments are removed, except when quoted", {

  sql <- read_sql(testthat::test_path("testfiles",
                                      "test_read.SQL"))
  # Expect 1 inline comment
  expect_equal(
    sum(stringr::str_detect( sql$sql, "--" ) ),
    1
  )

  # Expect 1 block comment
  expect_equal(
    sum(stringr::str_detect( sql$sql, "/\\*.*?\\*/" ) ),
    1
  )

  expect_equal(
    sum(stringr::str_detect(
      unlist(
        stringr::str_replace_all( sql$sql, # Take out all the quotes...
                                "'.*?'",
                                "")
        ),
      "--") # ... and then check for inline comments
      ),
    0
  )
  expect_equal(
    sum(stringr::str_detect(
      unlist(
        stringr::str_replace_all( sql$sql, # Take out all the quotes...
                                  "'.*?'",
                                  "")
      ),
      "/\\*.*?\\*/")), # ... and then check for block comments
    0
  )

})

test_that("Comments are correctly interpreted", {
  sql <- read_sql(testthat::test_path("testfiles",
                                      "test_read.SQL"))

  expect_equal(names(sql),
               c("qname",
                 "quotesql",
                 "interpolate",
                 "execmethod",
                 "geometry",
                 "conn_name",
                 "sql",
                 "filename"))

  expect_equal(sql$qname,
               c("showtabs",
                 "sample",
                 "quoted_inline",
                 "quoted_block")
               )

  expect_equal(sql$filename[[1]], "test_read")

  expect_equal(sum(is.na(sql$quotesql)), 4)

  expect_equal(sum(is.na(sql$interpolate)), 4)

  expect_equal(sum(is.na(sql$execmethod)), 2)
  expect_equal(sql$execmethod[3:4], c("spatial","spatial"))

  expect_equal(sum(is.na(sql$geometry)), 2)
  expect_equal(sql$geometry[3:4], c("mystring", "mystring"))

  expect_equal(sum(is.na(sql$conn_name)), 0)
  expect_equal(sql$conn_name[1:2], c("a_connection","a_connection"))
  expect_equal(sql$conn_name[3:4], c("another_connection", "another_connection"))

  # Test cascade gets turned off
  sql2 <- read_sql(testthat::test_path("testfiles",
                                      "test_read.SQL"),
                  cascade = FALSE)

  expect_equal(sum(is.na(sql2$execmethod)), 3)
  expect_equal(sql2$execmethod[3], "spatial")

  expect_equal(sum(is.na(sql2$geometry)), 3)
  expect_equal(sql2$geometry[3], "mystring")

  expect_equal(sum(is.na(sql2$conn_name)), 2)
  expect_equal(sql2$conn_name[1], c("a_connection"))
  expect_equal(sql2$conn_name[3], c("another_connection"))

})

test_that("Errors will be raised for unknown interpreted comment values", {
  expect_error(read_sql(testthat::test_path("testfiles",
                                            "unknown_quotesql.SQL")))

  expect_error(read_sql(testthat::test_path("testfiles",
                                            "unknown_interpolate.SQL")))

  expect_error(read_sql(testthat::test_path("testfiles",
                                            "unknown_execmethod.SQL")))
})
