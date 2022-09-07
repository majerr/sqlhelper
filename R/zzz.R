.onLoad <- function(libname,pkgname){
  assign("connection_cache",
         new.env(parent = emptyenv()),
         parent.env(environment()))

  no_connect <- options("SQLHELPER_NO_CONNECT_ON_LOAD")[[1]]
  if(is.null(no_connect)){
    create_connections()

  } else if(no_connect == FALSE) { # unlikely!
    create_connections()
  }
}
