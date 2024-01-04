-- qname = create_setosa_table
-- execmethod = execute
-- conn_name = sqlite_simple
CREATE TABLE iris_setosa as SELECT * FROM IRIS WHERE SPECIES = 'setosa';

-- qname = get_setosa_table
-- execmethod = get
-- conn_name = sqlite_simple
SELECT * FROM iris_setosa;
