# sqlhelper (development version)

## Enhancements

* renamed `runqueries()` and `runfiles()` as `run_queries()` and `run_files()`
* added `runqueries()` and `runfiles()` as aliases for `run_queries()` and `run_files()`  
* numerous documentation updates
* `run_files()` no longer requires an explicit `cascade` argument 

## Bug fixes

* read/prep functions now pass dots instead of duplicating default values

# sqlhelper 0.1.2

## Bug fixes

* `runfiles()` error preparing multiple files (#1)

# sqlhelper 0.1.1

* Added a `NEWS.md` file to track changes to the package.
