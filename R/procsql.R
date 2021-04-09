# PROC SQL hahahahaha
# Various internal sql processing functions

# read_sql_file and split_sql originated with Tony Breyal's sql processing functions
# and were refactored by Matthew Roberts to allow comment interpretation
read_sql_file <- function(file_path)  {

  # read lines in file.
  lines <- readLines(file_path, warn=FALSE)
  # Extract the comments
  interpreted_comments <- interpret_comments(lines)

  #remove blanks lines
  lines <- trimws(lines)
  lines <- lines[nchar(lines) > 0] # Drop blank lines

  # Remove comments
  lines <- gsub(pattern = "(.*?)--.*", replacement = "\\1", x = lines )
  lines <- remove_block_comments(lines)

  # Collpase lines into a single character vector.
  sql_code <- paste0(lines, collapse = " ")

  # Collapse repeated whitespace.
  sql_code <- gsub("\\s+", " ", x = sql_code, perl = TRUE);

  queries <- as.list(split_sql(sql_code))
  names(queries) <- interpreted_comments$qnames

  # Return a list containing the db.
  return(list("db"=interpreted_comments$db,"queries"=queries))
}

remove_block_comments <- function(lines){
  ## linetok is a low entropy token to temporarily replace newlines
  linetok <- paste(sample(c(letters,LETTERS,0:9),100,replace=TRUE),collapse="")
  all_code <- paste(lines,collapse=linetok)
  no_comments <- gsub(pattern = "/\\*.*?\\*/", replacement = linetok, x=all_code)
  #no_comments <- gsub(pattern = "/\\*.*\\*/", replacement = linetok, x=all_code)
  unlist(strsplit(no_comments,split=linetok))
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
  all_code <- paste(lines,collapse=linetok)

  blocks <- unlist(strsplit(all_code,split=";"))

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

