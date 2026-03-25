# Changelog

## sqlhelper (development version)

## sqlhelper 0.2.1

CRAN release: 2024-01-21

### submission fixes

- quote ‘SQL’ in title and description
- removed output via print()/cat(). Now only message()/warning()/stop()
  are used
- removed if(FALSE) from examples
- fixed codecov badge

## sqlhelper 0.2.0

### Enhancements

- connection configs now inherit characteristics from earlier configs
  with the same name (i.e. site-wide configs may be tweaked by users)
  ([\#9](https://github.com/majerr/sqlhelper/issues/9))
- [`config_examples()`](https://majerr.github.io/sqlhelper/dev/reference/config_examples.md)
  added to provide example connection yaml strings
- renamed
  [`runqueries()`](https://majerr.github.io/sqlhelper/dev/reference/run_queries.md)
  and
  [`runfiles()`](https://majerr.github.io/sqlhelper/dev/reference/run_files.md)
  as
  [`run_queries()`](https://majerr.github.io/sqlhelper/dev/reference/run_queries.md)
  and
  [`run_files()`](https://majerr.github.io/sqlhelper/dev/reference/run_files.md)
- added
  [`runqueries()`](https://majerr.github.io/sqlhelper/dev/reference/run_queries.md)
  and
  [`runfiles()`](https://majerr.github.io/sqlhelper/dev/reference/run_files.md)
  as aliases for
  [`run_queries()`](https://majerr.github.io/sqlhelper/dev/reference/run_queries.md)
  and
  [`run_files()`](https://majerr.github.io/sqlhelper/dev/reference/run_files.md)
- many documentation updates, including:
  - [`vignette("sqlhelper")`](https://majerr.github.io/sqlhelper/dev/articles/sqlhelper.md)
    updated
  - [`vignette("connections")`](https://majerr.github.io/sqlhelper/dev/articles/connections.md)
    updated
  - [`vignette("execution")`](https://majerr.github.io/sqlhelper/dev/articles/execution.md)
    added
  - [`vignette("use_case")`](https://majerr.github.io/sqlhelper/dev/articles/use_case.md)
    added ([\#14](https://github.com/majerr/sqlhelper/issues/14))
- [`read_sql()`](https://majerr.github.io/sqlhelper/dev/reference/read_sql.md)
  now accepts a flag to turn off cascade behaviour
  ([\#7](https://github.com/majerr/sqlhelper/issues/7))
- [`run_files()`](https://majerr.github.io/sqlhelper/dev/reference/run_files.md)
  no longer requires an explicit `cascade` argument
- [`run_files()`](https://majerr.github.io/sqlhelper/dev/reference/run_files.md)
  and
  [`run_queries()`](https://majerr.github.io/sqlhelper/dev/reference/run_queries.md)
  now attempt to connect lazily using the config search path
  ([\#8](https://github.com/majerr/sqlhelper/issues/8))

### Bug fixes

- read/prep functions now pass dots instead of duplicating default
  values ([\#6](https://github.com/majerr/sqlhelper/issues/6))
- [`not_connected()`](https://majerr.github.io/sqlhelper/dev/reference/is_connected.md)
  no longer errors when passed a missing connection name
  ([\#10](https://github.com/majerr/sqlhelper/issues/10))

## sqlhelper 0.1.2

### Bug fixes

- [`runfiles()`](https://majerr.github.io/sqlhelper/dev/reference/run_files.md)
  error preparing multiple files
  ([\#1](https://github.com/majerr/sqlhelper/issues/1))

## sqlhelper 0.1.1

- Added a `NEWS.md` file to track changes to the package.
