-- Test interpretation and stripping of comment lines.


-- conn_name=a_connection
-- ------------------------------------------------------------

-- what tables have we got?
-- qname=showtabs
SELECT name FROM

/* a multi-
line comment in the
middle of the
block*/

sqlite_schema WHERE type='table';

--------------------------------------------------------------;

-- qname=sample
select * -- with a comment on a code line
-- and another on a line by itself in the middle of a block
from iris limit 10;

/* a multi-
line comment at
the end of the
block*/

-- ------------------------------------------------------------

-- conn_name = another_connection
-- execmethod = spatial
-- geometry = mystring
-- make sure quoted double dashes survive the comment strip
-- qname=quoted_inline
select
'stringvar' as myString
from iris
where 'foo' = '-- oops';

/* a single-line block comment */

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
