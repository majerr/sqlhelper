# To extend the list of internals add to vars and source() this
# file

vars <- list(
  "connection_template" = list("conn"=NA,
                               "driver"=NA,
                               "pool"=FALSE,
                               "conn_str"=NA,
                               "description"=NA),

  "conf_fn" = "sqlhelper_db_conf.yml",

  "interpretable_names" = c("qname",
                          "quotesql",
                          "interpolate",
                          "execmethod",
                          "geometry",
                          "conn_name"),

  "recognized_methods" = c("get",
                          "execute",
                          "sendq",
                          "sends",
                          "spatial"),

  "sql_tbl_names" = c("qname",
                    "quotesql",
                    "interpolate",
                    "execmethod",
                    "geometry",
                    "conn_name",
                    "sql",
                    "filename"),

  # acceptable driver_types found in yml configs
  "driver_types" = c("odbc",
                "sqlserver",
                "sqlite",
                "postgresql",
                "mysql",
                "mariadb",
                "bigquery"),

  # packages corresponding to db_types
  "driver_pkgs" = c("odbc",
                    "odbc",
                    "RSQLite",
                    "RPostgres",
                    "RMariaDB",
                    "RMariaDB",
                    "bigrquery"),

  # functions in db_driver_pkgs corresponding to db_types
  "driver_funcs" = c("odbc",
                     "odbc",
                       "SQLite",
                       "Postgres",
                       "MariaDB",
                       "MariaDB",
                       "bigquery")

)

list2env(vars,.GlobalEnv)
save(list=names(vars), file="R/sysdata.rda")
rm(list=c(names(vars),"vars"))
