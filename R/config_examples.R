#' Examples of yaml configurations for database connections
#'
#' Provides example configurations for several databases and a range of options
#'
#' Irrespective of whether a filename is supplied, yaml configuration examples
#' will be returned invisibly as a single string and printed if the session is
#' interactive.
#'
#' @param filename A string. If supplied, examples are written to a file with this name.
#'
#' @returns A yaml string of database configuration examples, invisibly.
#'
#' @examples
#' config_examples()
#'
#' # write the examples to a temporary file called 'examples.yml'
#' config_examples(file.path(tempdir(), "examples.yml"))
#'
#' @export
config_examples <- function(filename = NA){
  example_lines <- system.file("examples",
                          "config_examples.yml",
                          package="sqlhelper") |>
    readLines()

  if(!is.na(filename)){
    stopifnot(is.character(filename))
    writeLines(example_lines,filename)
  }

  stringr::str_c(example_lines,
                 collapse = "\n")
}
