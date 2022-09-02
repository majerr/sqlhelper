.onLoad <- function(libname,pkgname){
  assign("connection_cache",
         new.env(parent = emptyenv()),
         parent.env(environment()))
  create_connections()
}
