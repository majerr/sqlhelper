-- Run some queries on cds


-- ------------------------------------------------------------

-- switch to the COVID19 database
-- qname=usedb
use COVID19;

-- ------------------------------------------------------------

-- what tables have we got?
-- qname=showtabs
select * from

/* a multi-
line comment in the
middle of the
block*/

INFORMATION_SCHEMA.TABLES;

-- ------------------------------------------------------------

-- try out a generator
-- qname=sample
select * -- with a comment on a code line
-- and another on a line by itself in the middle of a block
-- from [COVID19].[Analyse].[vw_COVIDKPI_LRFCDR_Latest] where {nthofk(1000,"[document.id]")};
from [Analyse].[vw_COVIDKPI_LRFCDR_Latest] where {nthofk(1000,"[document.id]")};

/* a multi-
line comment at
the end of the
block*/


-- ------------------------------------------------------------

-- make sure quoted double dashes survive the comment strip
-- qname=quoted_doubledash
select
'stringvar' as myString
from [Analyse].[vw_COVIDKPI_LRFCDR_Latest]
where 'foo' = '-- oops';

-- ------------------------------------------------------------



-- a few single
-- line comments at the
-- very end

