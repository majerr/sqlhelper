-- Run some queries on hive
--db =   hIve

-- ------------------------------------------------------------

-- switch to the uc database
-- qname=usedb
use uc;

-- ------------------------------------------------------------

-- what tables have we got?
-- qname=showtabs
show tables;

/* a multi-
line comment in the
middle of the
file*/

-- ------------------------------------------------------------

-- try out a generator
-- qname=sample
select * -- with a comment on a code line
-- and another on a line by itself in the middle of a block
from contract_dim_v where {nthofk(1000,"contract_key")};

/* a multi-
line comment at
the end of the
file*/

-- ------------------------------------------------------------

/* Mix Hivevars with interpolations */
-- qname=set_hivevar
set hivevar:maxdate = '2017-04-01';

-- qname=test_hivevar
select * -- with a comment on a code line
-- and another on a line by itself in the middle of a block
from contract_dim_v
where {nthofk(1000,"contract_key")}
and start_date <= ${{hivevar:maxdate}};


-- a few single
-- line comments at the
-- very end

