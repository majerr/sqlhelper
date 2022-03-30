# PROC SQL hahahahaha
# Various internal sql processing functions

# read_sql_file and split_sql originated with Tony Breyal's sql processing functions
# and were refactored by Matthew Roberts to allow comment interpretation
read_sql_file <- function(file_path)  {
  # read lines in file.
  lines <- readLines(file_path, warn=FALSE)
  # Extract the comments
  interpreted_comments <- interpret_comments(lines)
  lines <- remove_block_comments(lines)
  lines <- remove_inline_comments(lines)

  #remove blanks lines
  lines <- trimws(lines)
  lines <- lines[nchar(lines) > 0] # Drop blank lines

  # Collpase lines into a single character vector.
  sql_code <- paste0(lines, collapse = " ")

  # Collapse repeated whitespace.
  sql_code <- gsub("\\s+", " ", x = sql_code, perl = TRUE);

  queries <- as.list(split_sql(sql_code))
  names(queries) <- interpreted_comments$qnames

  # Return a list containing the db.
  return(list("db"=interpreted_comments$db,"queries"=queries))
}


# Generate a low entropy token to temporarily replace newlines
tok <- function(){
  paste(sample(c(letters,LETTERS,0:9),100,replace=TRUE),collapse="")
}

remove_block_comments <- function(lines){
  linetok <- tok()
  all_sql <- paste(lines,collapse=linetok)
  block_pattern <- "/\\*.*?\\*/"
  unlist(
    strsplit(
      stringr::str_replace_all(string = all_sql,
                               pattern = block_pattern,
                               replacement = linetok),
      split=linetok
    )
  )
}

remove_inline_comments <- function(lines){
  linetok <- tok()
  all_ql <- paste(lines,collapse=linetok)
  string_pattern <- "\\'.*?\\'"
  n <- stringr::str_count(all_ql,
                          string_pattern)

  if(n>0){
    str_replacements <- replicate(
      n,
      tok()
    )
    names(str_replacements) <- unlist(
      stringr::str_extract_all(
        string = all_ql,
        pattern = string_pattern)
    )

    no_strs <- stringr::str_replace_all(
      string = all_ql,
      str_replacements)
  } else {
    no_strs <- all_ql
  }

  no_str_lines <- unlist(
    strsplit(
      no_strs,
      split=linetok)
  )

  inline_pattern = "(.*?)--.*"
  no_inlines <- paste(
    gsub(
      pattern = inline_pattern,
      replacement = "\\1",
      x = no_str_lines),
    collapse = linetok)

  if(n>0){
    reinstatements <- names(str_replacements)
    names(reinstatements) <- str_replacements
    no_comments <- stringr::str_replace_all(
      string = no_inlines,
      reinstatements)
  } else {
    no_comments <- no_inlines
  }

  unlist(
    strsplit(
      no_comments,
      split=linetok
    )
  )
}

# take a bunch of lines and extract any actionable comments.
# returns list(db='hive'|'pg'|NA, names=c(...))
# where db and names are extracted from comments
interpret_comments <- function(lines){

  lines <- remove_block_comments(lines[nchar(lines) > 0])

  # semi-colons in comments confuse the sql block divisions.
  # They're not interpretable, so we drop them here.
  lines <- lines[grepl(pattern="--.*;.*",x=lines)==FALSE]
  lines <- gsub(pattern = "(.+?)--.*", replacement = "\\1", x = lines )

  # drop blank lines
  lines <- trimws(lines)
  lines <- lines[lines != ""]

  # linetok is a low entropy token to temporarily replace newlines
  # this is to allow spliting into blocks on ';'
  linetok <- paste(sample(c(letters,LETTERS,0:9),100,replace=TRUE),collapse="")
  all_ql <- paste(lines,collapse=linetok)

  blocks <- unlist(strsplit(all_ql,split=";"))

  # Drop comment-only blocks.
  # Zero length queries are dropped before running (to avoid the overhead)
  # so if we don't drop them here too, then we get more query names than queries
  blocklines <- strsplit(blocks,split=linetok)

  blockslist <- lapply(blocklines,
                       function(x){x <- gsub(pattern = "^\\s*--.*",
                                             replacement = "",
                                             x=x)
                                   x <- x[x != ""]
                                   paste(x, collapse=linetok)})
  codeblocks <- unlist(blockslist)
  blocks <- blocks[codeblocks != ""]

  # Extract names from comments
  # NB lazy regex evaluation required to avoid matching double newlines
  qnames <- gsub(pattern=stringr::str_interp(".*--\\s*qname\\s*=\\s*(\\S+?)${linetok}.*"),
                replacement="\\1",
                x=blocks)
  qnames[qnames == blocks] = ""

  # Extract db from comments
  # NB lazy regex evaluation required to avoid matching double newlines
  db <-  gsub(pattern=stringr::str_interp(".*--\\s*db\\s*=\\s*(\\S+?)\\s*${linetok}.*"),
              replacement="\\1",
               x=blocks[1])
  if(db == blocks[1]){
    db=NA
  }

  return(list("db"=db,"qnames"=qnames))
}

# Take a block of SQL and split into individual SQL queries by using ";" as the seperator.
split_sql <- function(sql_statements) {

  # Split SQL on ";".
  sql <- unlist(strsplit(sql_statements, split = ";"))

  # Remove empty queries.
  sql <- sql[nchar(sql) > 1L]

  # Put semmcolon at end of each query.
  sql <- paste(trimws(sql), ";")

  # Return vector of SQL queries
  return(sql)
}

