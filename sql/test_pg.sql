
--  db =PostgreSQL

-- What databases are available?
-- qname=dbnames
select datname from pg_database;

-- what tables have we got in the current database?
-- qname=tabnames
select tablename
from pg_catalog.pg_tables
where
  schemaname != 'pg_catalog' AND
  schemaname != 'information_schema';

/* a multi-
line comment at
the end of the
file*/
