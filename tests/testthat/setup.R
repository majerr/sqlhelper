op <- options("SQLHELPER_NO_CONNECT_ON_LOAD" = TRUE)
withr::defer(options(op), teardown_env())
