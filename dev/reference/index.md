# Package index

## Execution

Functions for reading, preparing and executing SQL

- [`run_files()`](https://majerr.github.io/sqlhelper/dev/reference/run_files.md)
  [`runfiles()`](https://majerr.github.io/sqlhelper/dev/reference/run_files.md)
  : Read, prepare and execute .SQL files
- [`run_queries()`](https://majerr.github.io/sqlhelper/dev/reference/run_queries.md)
  [`runqueries()`](https://majerr.github.io/sqlhelper/dev/reference/run_queries.md)
  : Execute a sequence of SQL queries
- [`read_sql()`](https://majerr.github.io/sqlhelper/dev/reference/read_sql.md)
  : Read a sql file and return it's contents as a tibble
- [`prepare_sql()`](https://majerr.github.io/sqlhelper/dev/reference/prepare_sql.md)
  : prepare queries and assemble meta data prior to execution

## Connections

Functions for creating and managing database connections

- [`connect()`](https://majerr.github.io/sqlhelper/dev/reference/connect.md)
  : (Re-)Establish connections to databases
- [`config_examples()`](https://majerr.github.io/sqlhelper/dev/reference/config_examples.md)
  : Examples of yaml configurations for database connections
- [`connection_info()`](https://majerr.github.io/sqlhelper/dev/reference/connection_info.md)
  : Browse available connections
- [`live_connection()`](https://majerr.github.io/sqlhelper/dev/reference/live_connection.md)
  : Return the named connection or NULL
- [`set_default_conn_name()`](https://majerr.github.io/sqlhelper/dev/reference/set_default_conn_name.md)
  [`get_default_conn_name()`](https://majerr.github.io/sqlhelper/dev/reference/set_default_conn_name.md)
  : Set/get the name of the default connection to use
- [`default_conn()`](https://majerr.github.io/sqlhelper/dev/reference/default_conn.md)
  : Return the default connection
- [`is_connected()`](https://majerr.github.io/sqlhelper/dev/reference/is_connected.md)
  [`not_connected()`](https://majerr.github.io/sqlhelper/dev/reference/is_connected.md)
  : Test whether a database is connected
- [`disconnect()`](https://majerr.github.io/sqlhelper/dev/reference/disconnect.md)
  : Close all connections and remove them from the connections cache
