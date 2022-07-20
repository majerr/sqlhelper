.onLoad <- function(libname,pkgname){
  set_connections()
  conn_cache()
}

.onUnload <- function(libname,pkgname){
  close_connections()
}
