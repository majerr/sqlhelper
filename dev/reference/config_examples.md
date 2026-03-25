# Examples of yaml configurations for database connections

Provides example configurations for several databases and a range of
options

## Usage

``` r
config_examples(filename = NA)
```

## Arguments

- filename:

  A string. If supplied, examples are written to a file with this name.

## Value

A yaml string of database configuration examples, invisibly.

## Details

Irrespective of whether a filename is supplied, yaml configuration
examples will be returned invisibly as a single string and printed if
the session is interactive.

## Examples

``` r
config_examples()
#> [1] "sqlite_simple:\n  driver_type: sqlite\n  connection:\n    Server: \":memory:\"\n\nsqlite_pool:\n  driver_type: sqlite\n  pool: yes\n  connection:\n    Server: \":memory:\"\n\nsqlserver_simple:\n  driver_type: sqlserver\n  description: >\n    A one line description to remind you what this server is for\n  connection:\n    Driver: \"{ODBC Driver 17 for SQL Server}\" # See https://connectionstrings.com for other examples\n    Server: \"the_server_hostname\"\n    Trusted_Connection: \"yes\""

# write the examples to a temporary file called 'examples.yml'
config_examples(file.path(tempdir(), "examples.yml"))
#> [1] "sqlite_simple:\n  driver_type: sqlite\n  connection:\n    Server: \":memory:\"\n\nsqlite_pool:\n  driver_type: sqlite\n  pool: yes\n  connection:\n    Server: \":memory:\"\n\nsqlserver_simple:\n  driver_type: sqlserver\n  description: >\n    A one line description to remind you what this server is for\n  connection:\n    Driver: \"{ODBC Driver 17 for SQL Server}\" # See https://connectionstrings.com for other examples\n    Server: \"the_server_hostname\"\n    Trusted_Connection: \"yes\""
```
