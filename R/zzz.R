.onLoad <- function(libname,pkgname){
  set_cache_env()

  set_connections()
}

.onUnload <- function(libname,pkgname){
  close_connections()
  rm("sqlhelper_conn_cache", envir = get_cache_env())
}
