-- qname = first_query
-- conn_name = not_the_default
select * from foo;

--qname = second_query
-- this query will also be executed against the
-- connection called 'not_the_default'
select * from bar;
