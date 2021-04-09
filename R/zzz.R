.onLoad <- function(libname,pkgname){
  set_connections()
}

.onUnload <- function(libname,pkgname){
  close_connections()
}
