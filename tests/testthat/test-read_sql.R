test_that("The correct number of queries are read", {
  sql <- read_sql(testthat::test_path("testfiles",
                                      "test_read.sql"))
  expect_equal(nrow(sql),4)
})

test_that("Comments are removed, except when quoted", {

  sql <- read_sql(testthat::test_path("testfiles",
                                      "test_read.sql"))
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
                                      "test_read.sql"))

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

  expect_equal(sum(is.na(sql$quotesql)), 4)

  expect_equal(sum(is.na(sql$interpolate)), 4)

  expect_equal(sum(is.na(sql$execmethod)), 2)
  expect_equal(sql$execmethod[3:4], c("spatial","spatial"))

  expect_equal(sum(is.na(sql$geometry)), 2)
  expect_equal(sql$geometry[3:4], c("mystring", "mystring"))

  expect_equal(sum(is.na(sql$conn_name)), 0)
  expect_equal(sql$conn_name[1:2], c("a_connection","a_connection"))
  expect_equal(sql$conn_name[3:4], c("another_connection", "another_connection"))

})

test_that("Errors will be raised for unknown interpreted comment values", {
  expect_error(read_sql(testthat::test_path("testfiles",
                                            "unknown_quotesql.sql")))

  expect_error(read_sql(testthat::test_path("testfiles",
                                            "unknown_interpolate.sql")))

  expect_error(read_sql(testthat::test_path("testfiles",
                                            "unknown_execmethod.sql")))
})
