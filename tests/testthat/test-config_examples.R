test_that("config_examples returns the correct string", {
  example_lines <- system.file("examples",
                               "config_examples.yml",
                               package="sqlhelper") |>
    readLines()

  examples <- stringr::str_c(example_lines,
                             collapse = "\n")

  expect_equal(config_examples(), examples)

  config_examples("written.yml")
  written_examples <- stringr::str_c(
    readLines("written.yml"),
    collapse = "\n"
  )
  file.remove("written.yml")
  expect_equal(examples, written_examples)

})
