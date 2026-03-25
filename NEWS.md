# sqlhelper (development version)

## Bug fixes

* `validate_configs()` now correctly drops invalid configurations; previously
  `!is.null()` applied to a list always returned `TRUE` so NULLs were never
  filtered (#21)
* `validate_config()` no longer requires a case-sensitive `Server` key;
  the earlier case-insensitive check is sufficient (#22)
* Fixed `conn_child_is_list` test fixture which was missing `driver_type`,
  preventing it from reaching the intended code path (#22)
* Driver-specific tests in `test-get_driver.R` now use `skip_if_not_installed()`
  guards, fixing CRAN check failures on platforms where suggested packages
  (e.g. `bigrquery`) are unavailable (#27)
* Updated GitHub Actions workflows from v3 to v4 (`actions/checkout`,
  `actions/upload-artifact`)

# sqlhelper 0.2.1

## submission fixes
* quote 'SQL' in title and description
* removed output via print()/cat(). Now only message()/warning()/stop() are used
* removed if(FALSE) from examples
* fixed codecov badge

# sqlhelper 0.2.0

## Enhancements

* connection configs now inherit characteristics from earlier configs with the same name
(i.e. site-wide configs may be tweaked by users) (#9)
* `config_examples()` added to provide example connection yaml strings
* renamed `runqueries()` and `runfiles()` as `run_queries()` and `run_files()`
* added `runqueries()` and `runfiles()` as aliases for `run_queries()` and
`run_files()`
* many documentation updates, including:
  - `vignette("sqlhelper")` updated
  - `vignette("connections")` updated
  - `vignette("execution")` added
  - `vignette("use_case")` added (#14)
* `read_sql()` now accepts a flag to turn off cascade behaviour (#7)
* `run_files()` no longer requires an explicit `cascade` argument 
* `run_files()` and `run_queries()` now attempt to connect lazily using the
config search path (#8)

## Bug fixes

* read/prep functions now pass dots instead of duplicating default values (#6)
* `not_connected()` no longer errors when passed a missing connection name (#10)

# sqlhelper 0.1.2

## Bug fixes

* `runfiles()` error preparing multiple files (#1)

# sqlhelper 0.1.1

* Added a `NEWS.md` file to track changes to the package.
