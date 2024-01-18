

-- qname=t1
-- execmethod = execute
CREATE TABLE iris1 as select * from iris limit 100;

-- qname = t2
CREATE TABLE iris2 as select * from iris1 limit 50;

-- qname = t3
CREATE TABLE iris3 as select * from iris2 limit 10;
