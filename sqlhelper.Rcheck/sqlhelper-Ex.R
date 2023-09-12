pkgname <- "sqlhelper"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
options(pager = "console")
base::assign(".ExTimings", "sqlhelper-Ex.timings", pos = 'CheckExEnv')
base::cat("name\tuser\tsystem\telapsed\n", file=base::get(".ExTimings", pos = 'CheckExEnv'))
base::assign(".format_ptime",
function(x) {
  if(!is.na(x[4L])) x[1L] <- x[1L] + x[4L]
  if(!is.na(x[5L])) x[2L] <- x[2L] + x[5L]
  options(OutDec = '.')
  format(x[1L:3L], digits = 7L)
},
pos = 'CheckExEnv')

### * </HEADER>
library('sqlhelper')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("config_examples")
### * config_examples

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: config_examples
### Title: Examples of yaml configurations for database connections
### Aliases: config_examples

### ** Examples

config_examples()

# to write the examples to a file called 'examples.yml'
## Not run:  config_examples("examples.yml")




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("config_examples", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("connect")
### * connect

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: connect
### Title: (Re-)Establish connections to databases
### Aliases: connect

### ** Examples

## Not run: 
##D # Search for config files in rappdirs::site_config_dir(),
##D # rappdirs::user_config_dir(), and read `my_connections.yml` in the current
##D # working directory
##D connect("my_connections.yml")
##D 
##D # Read only `my_connections.yml` in the current working directory
##D connect("my_connections.yml", exclusive=TRUE)
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("connect", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("read_sql")
### * read_sql

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: read_sql
### Title: Read a sql file and return it's contents as a tibble
### Aliases: read_sql

### ** Examples


library(sqlhelper)

fn <- system.file( "examples/read_sql.sql", package="sqlhelper" )
readLines( fn ) |> writeLines()

connect( system.file("examples/sqlhelper_db_conf.yml", package="sqlhelper") )
read_sql(fn)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("read_sql", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("runqueries")
### * runqueries

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: runqueries
### Title: Execute a sequence of SQL queries
### Aliases: runqueries run_queries

### ** Examples

{
library(sqlhelper)

connect(
    system.file("examples/sqlhelper_db_conf.yml", package="sqlhelper"),
    exclusive=TRUE)

DBI::dbWriteTable( default_conn(),
                  "iris",
                  iris)

n <- 5

runqueries(
    c(top_n = "select * from iris limit {n}", # interpolation is controlled
                                              # with the 'values' argument
      uniqs = "select distinct species as species from iris")
)

## Use the execmethod parameter if you don't want to return results
runqueries("create table iris_setosa as select * from iris where species = 'setosa'",
          execmethod = 'execute')

runqueries("select distinct species as species from iris_setosa")
}



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("runqueries", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
