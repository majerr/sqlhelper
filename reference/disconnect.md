# Close all connections and remove them from the connections cache

Close all connections and remove them from the connections cache

## Usage

``` r
disconnect()
```

## Value

`NULL`, invisibly

## Examples

``` r
library(sqlhelper)
connect(
  system.file("examples",
              "sqlhelper_db_conf.yml",
              package="sqlhelper")
)
disconnect()
```
