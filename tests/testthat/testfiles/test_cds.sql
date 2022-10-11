-- Run some queries against the newly-created iris table


-- conn_name=single_mem
-- ------------------------------------------------------------

-- what tables have we got?
-- qname=showtabs
SELECT name FROM

/* a multi-
line comment in the
middle of the
block*/

sqlite_schema WHERE type='table';

-- ------------------------------------------------------------

-- qname=sample
select * -- with a comment on a code line
-- and another on a line by itself in the middle of a block
from iris limit 10;

/* a multi-
line comment at
the end of the
block*/

-- ------------------------------------------------------------

-- conn_name = cds
-- execmethod = spatial
-- geometry = mystring
-- make sure quoted double dashes survive the comment strip
-- qname=quoted_doubledash
select
'stringvar' as myString
from iris
where 'foo' = '-- oops';

/* as single-line block comment */

-- ------------------------------------------------------------

-- make sure quoted block comments survive the comment strip
-- qname=quoted_block
select
'stringvar' as myString
from iris
where 'foo' = ' /* KEEP ME! */ ';

-- ------------------------------------------------------------


-- a few single
-- line comments at the
-- very end
