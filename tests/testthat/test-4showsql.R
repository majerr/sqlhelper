context("showsql")
library(ucsqlhelper)

hfn <<- "../../sql/test_hive.sql"
db <<- "uc"
tab <<- "contract_dim_v"
nth <<- 1000

test_that("showsql returns properly interpreted queries when supplied with queries",{
  sql <- showsql(c(usedb="use {db}",contract="select * from {tab} where {nthofk(nth,'contract_key')}"),is.queries=TRUE)
  expect_equal(sql$usedb,glue::glue("use uc"))
  expect_equal(sql$contract, glue::glue("select * from contract_dim_v where abs(hash(contract_key)) % 1000 < 1"))
})

rm(hfn,db,tab,nth,pos=.GlobalEnv)
