#' Read a sql file and return it's contents as a tibble
#'
#' @param file_name Full name and path of a file to read
#' @param cascade Parameters for executing each query may be specified as
#'   comments in the SQL file. If `cascade=TRUE`, execution parameters specified
#'   in the file will be cascaded to subsequent queries where that parameter is
#'   not specified. This enables you to set a parameter (e.g. the connection
#'   name) once, for the first query in a file, and use it for all the
#'   subsequent queries.
#'
#' @return A tibble containing 1 row per query with the following fields:
#' \describe{
#'  \item{qname}{character. A name for this query}
#'  \item{quotesql}{"yes" or "no". Should parameterized character values be quoted for this query?}
#'  \item{interpolate}{"yes" or "no". Should this query be parameterized with values from R?}
#'  \item{execmethod}{The method to execute this query.
#'  One of "get" ([DBI::dbGetQuery()]), "execute" ([DBI::dbExecute()]), "sendq" ([DBI::dbSendQuery()]), "sends" ([DBI::dbSendStatement()]) or "spatial" ([sf::st_read()])}
#'  \item{geometry}{character. If `execmethod` is "spatial", which is the geometry column?}
#'  \item{conn_name}{character. The name of the database connection to use for this query.
#'  Must be the name of a configured sqlhelper connection.}
#'  \item{sql}{The sql query to be executed}
#'  \item{filename}{The value of `file_name`}
#' }
#'
#' @details Multiple SQL queries in files should be terminated by semi-colons
#'   (`;`), as usual.
#'
#'   The values of `qname`, `quotesql`, `interpolate`, `execmethod`, `geometry`,
#'   and `conn_name` in the output may be specified with comments immediately
#'   preceding each query (see examples).
#'
#'   With the exception of `qname`, the value of each interpreted comment is
#'   cascaded to subsequent queries (assuming `cascade=TRUE`). This means you
#'   may set values once for the first query in the file and they will apply to
#'   all the queries thereafter.
#'
#'   See [run_queries()] for the implications of setting execution parameters.
#'   See [prepare_sql()] for the treatment of missing values in the output and
#'   their defaults. The article `vignette("execution")` has further examples of
#'   using these parameters to control execution.
#'
#' @examples
#'
#' library(sqlhelper)
#'
#' fn <- system.file( "examples/read_sql_execution_params.SQL",
#'                    package="sqlhelper" )
#' readLines( fn ) |> writeLines()
#'
#' sql_tibble <- read_sql(fn)
#' sql_tibble
#' sql_tibble$sql
#'
#' fn <- system.file( "examples/read_sql_comments.SQL", package="sqlhelper" )
#' readLines( fn ) |> writeLines()
#'
#' sql_tibble <- read_sql(fn)
#' sql_tibble
#' sql_tibble$sql
#'
#' @export
read_sql <- function(file_name, cascade = TRUE)  {
  lines <- readLines(file_name, warn = FALSE)

  no_quoted_strings <- extract_quoted_strings(lines)

  no_blocks <- remove_block_comments(no_quoted_strings$lines)

  interpreted_comments <-
    interpret_comments(no_blocks, cascade = cascade)

  no_inlines <- remove_inline_comments(no_blocks)

  no_comments <-
    reinstate_quoted_strings(no_inlines, no_quoted_strings$id)

  no_ws <- trimws(no_comments)

  no_blanks <- no_ws[nchar(no_ws) > 0] # Drop blank lines

  sql_code <- gsub("\\s+",
                   " ",
                   x = paste0(no_blanks, collapse = " "),
                   perl = TRUE)


  # Add the sql and the filename to the tibble of interpreted comments
  interpreted_comments |>
    tibble::add_column(sql = split_sql(sql_code)) |>
    tibble::add_column(filename = gsub("\\..*", # strip extension ...
                                       "",
                                       basename(file_name))) # ... and path
}

#' Generate a low entropy token to temporarily replace newlines and quoted strings
#' @noRd
tok <- function() {
  paste(sample(c(letters, LETTERS, 0:9),
               100,
               replace = TRUE),
        collapse = "")
}

# Add a mutable quote cache to the package namespace
assign("quote_cache",
       new.env(parent = emptyenv()),
       environment())

#' Extract and cache single quoted strings from lines of sql
#'
#' @param lines A character vector containing consecutive lines of SQL code.
#' @noRd
extract_quoted_strings <- function(lines) {
  linetok <- tok()
  all_ql <- paste(lines,
                  collapse = linetok)

  string_pattern <- "\\'.*?\\'"

  n <- stringr::str_count(all_ql,
                          string_pattern)

  if (n > 0) {
    str_replacements <- replicate(n,
                                  tok())

    names(str_replacements) <- unlist(stringr::str_extract_all(string = all_ql,
                                                               pattern = string_pattern))

    no_strs <- stringr::str_replace_all(string = all_ql,
                                        stringr::fixed(str_replacements))

    id <- tok()

    assign(id,
           str_replacements,
           quote_cache)

  } else {
    no_strs <- all_ql
    id <- NULL
  }

  list("id" = id,
       "lines" = unlist(strsplit(no_strs,
                                 split = linetok)))
}

#' Replace tokens with their matched quoted strings in lines of sql
#'
#' @param lines A character vector containing consecutive lines of SQL code.
#' @noRd
reinstate_quoted_strings <- function(lines, id) {
  if (is.null(id)) {
    with_strs <- lines
  } else {
    linetok <- tok()
    all_lines <- paste(lines, collapse = linetok)

    reinstatements <- names(quote_cache[[id]])

    names(reinstatements) <- quote_cache[[id]]

    with_strs <- unlist(strsplit(
      stringr::str_replace_all(string = all_lines,
                               reinstatements),
      split = linetok
    ))
  }

  with_strs
}


remove_block_comments <- function(lines) {
  linetok <- tok()

  all_sql <- paste(lines, collapse = linetok)

  block_pattern <- "/\\*.*?\\*/"

  unlist(strsplit(
    stringr::str_replace_all(
      string = all_sql,
      pattern = block_pattern,
      replacement = linetok
    ),
    split = linetok
  ))
}

remove_inline_comments <- function(lines) {
  inline_pattern = "(.*?)--.*"

  gsub(pattern = inline_pattern,
       replacement = "\\1",
       x = lines)
}

comment_values <- function(comment_name, blocks, linetok) {
  values <-
    gsub(
      pattern = glue::glue(".*--\\s*{comment_name}\\s*=\\s*(\\S+?){linetok}.*"),
      replacement = "\\1",
      x = blocks
    )

  values[values == blocks] <- NA

  values
}

# Extract interpretable comments from lines.
interpret_comments <- function(lines, cascade) {
  # semi-colons in comments confuse the sql block divisions.
  # They're not interpretable, so we drop them here.
  lines <- lines[grepl(pattern = "--.*;.*", x = lines) == FALSE]

  lines <- gsub(pattern = "(.+?)--.*",
                replacement = "\\1",
                x = lines)

  # drop blank lines
  lines <- trimws(lines)

  lines <- lines[lines != ""]

  linetok <- tok()

  all_ql <- paste(lines,
                  collapse = linetok)

  blocks <- unlist(strsplit(all_ql, split = ";"))

  # Drop comment-only blocks.
  # Any zero length queries are dropped
  # so if we don't drop them here too, then we get more query names than queries
  blocklines <- strsplit(blocks, split = linetok)

  blockslist <- lapply(blocklines,
                       function(x) {
                         x <- gsub(pattern = "^\\s*--.*",
                                   replacement = "",
                                   x = x)
                         x <- x[x != ""]
                         paste(x, collapse = linetok)
                       })

  codeblocks <- unlist(blockslist)

  blocks <- blocks[codeblocks != ""]

  # interpretable_names is defined in data-raw/sysdata.r
  # it can be extended as needed
  interpreted_comments <- lapply(interpretable_names,
                                 comment_values,
                                 blocks,
                                 linetok)

  names(interpreted_comments) <- interpretable_names

  # Add more validation below as necessary

  #quotesql must be 'yes' or 'no'
  lapply(interpreted_comments$quotesql,
         function(x) {
           if ((!x %in% c("yes", "no")) && !is.na(x))
             stop(glue::glue("quotesql comments must be 'yes' or 'no', not {x}"))
         })

  #interpolate must be 'yes' or 'no'
  lapply(interpreted_comments$interpolate,
         function(x) {
           if ((!x %in% c("yes", "no")) && !is.na(x))
             stop(glue::glue("interpolate comments must be 'yes' or 'no', not {x}"))
         })

  # methods must be one of 'getquery', 'execute', 'sendquery', 'sendstatement' or 'spatial'
  lapply(interpreted_comments$execmethod,
         function(x) {
           if ((!x %in% recognized_methods) && !is.na(x))
             stop(
               glue::glue(
                 "execmethod must be one of {paste(recognized_methods, collapse=', ')}, not {x}"
               )
             )
         })

  # cascade interpreted comments to subsequent queries in the file
  interpreted_comments <- tibble::as_tibble(interpreted_comments)

  if (cascade)
    interpreted_comments <- tidyr::fill(
      interpreted_comments,
      "quotesql",
      "interpolate",
      "conn_name",
      "execmethod",
      "geometry",
      "conn_name"
    )

  interpreted_comments
}

# Take a block of SQL and split into individual SQL queries by using ";" as the seperator.
# split_sql originated with Tony Breyal's sql processing functions
split_sql <- function(sql_statements) {
  # Split SQL on ";".
  sql <- gsub(";", "",
              trimws(unlist(strsplit(
                sql_statements, split = ";"
              ))))

  # Remove empty queries.
  sql <- sql[nchar(sql) > 1L]

  # Put semicolon at end of each query.
  #sql <- paste(trimws(sql), ";",sep="")

  # Return vector of SQL queries
  return(sql)
}
