# (Re-)Establish connections to databases

Closes any open connections, reads config files as directed by
`config_filename` and `exclusive`, and creates new connections from the
descriptions in those files.

## Usage

``` r
connect(config_filename = NA, exclusive = FALSE)
```

## Arguments

- config_filename:

  String. The full name and path of a configuration file, or "site", or
  "user", or "example", or `NA` (the default). Cannot be `NA` if
  `exclusive = TRUE`.

- exclusive:

  Logical. If `TRUE`, the file named by `config_filename` is treated as
  the only config file. Site and user level files are not read. This
  parameter is ignored if `config_filename` is missing.

## Value

`NULL`, invisibly

## Details

If `exclusive=FALSE` (the default), configuration files will be sought
in the directory returned by
[`rappdirs::site_config_dir()`](https://rappdirs.r-lib.org/reference/site_data_dir.html),
the directory returned by
[`rappdirs::user_config_dir()`](https://rappdirs.r-lib.org/reference/user_data_dir.html),
and finally a file named by `config_filename` (if not `NA`). If elements
of those files conflict, later files overwrite the elements of earlier
files.

If `exclusive=TRUE`, only 1 file, indicated by the `config_filename`
parameter, will be read.

- If `config_filename = "site"`, a config file called
  `sqlhelper_db_conf.yml` will be sought in the directory returned by
  [`rappdirs::site_config_dir()`](https://rappdirs.r-lib.org/reference/site_data_dir.html)

- If `config_filename = "user"`, a config file called
  `sqlhelper_db_conf.yml` will be sought in the directory returned by
  [`rappdirs::user_config_dir()`](https://rappdirs.r-lib.org/reference/user_data_dir.html)

- If `config_filename` is not `NULL` (but not "site" or "user"), it is
  assumed to name a file.

A warning is raised if no valid configurations are found (e.g.
`connect()` is called without arguments and no site- or user-wide files
are present, or the connections in those files are invalid)

[`vignette("connections")`](https://majerr.github.io/sqlhelper/articles/connections.md)
explains how to write a config file and how to access the created
connections.

## Examples

``` r
library(sqlhelper)

example_filename <- system.file("examples",
                                "sqlhelper_db_conf.yml",
                                package = "sqlhelper")

# Search for config files in rappdirs::site_config_dir(),
# rappdirs::user_config_dir(), and read from example_filename
connect(example_filename)

# Read only the named example file
connect(example_filename, exclusive=TRUE)
```
