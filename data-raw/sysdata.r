# Adding internal placeholder vars to R/sysdata.rda means that they are found in
# the package env and don't get set in the user's global env.
#
# To extend the list of internal placeholders, add to vars and source() this
# file

vars <- list(
  "connection_template" = list("conn"=NA,
                               "driver"=NA,
                               "pool"=FALSE,
                               "conn_str"=NA,
                               "description"=NA),
  "conf_fn" = "sqlhelper_db_conf.yml"
)

list2env(vars,.GlobalEnv)
save(list=names(vars), file="R/sysdata.rda")
rm(list=c(names(vars),"vars"))
