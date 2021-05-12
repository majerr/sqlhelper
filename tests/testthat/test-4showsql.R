context("showsql")
library(sqlhelper)

hfn <<- "../../sql/test_cds.sql"
db <<- "[COVID19]"
tab <<- "[Analyse].[vw_COVIDKPI_LRFCDR_Latest]"
nth <<- 1000

test_that("showsql returns properly interpreted queries when supplied with queries",{
  sql <- showsql(c(usedb="use {db}",contract="select * from {db}.{tab} where {nthofk(nth,'[document.id]')}"),is.queries=TRUE)
  expect_equal(sql$usedb,glue::glue("use [COVID19]"))
  expect_equal(sql$contract, glue::glue("select * from [COVID19].[Analyse].[vw_COVIDKPI_LRFCDR_Latest] where abs(binary_checksum([document.id])) % 1000 < 1"))
})

rm(hfn,db,tab,nth,pos=.GlobalEnv)
