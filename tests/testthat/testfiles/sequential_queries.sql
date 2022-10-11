

-- qname=t1
-- conn = pool_mem
CREATE TABLE iris1 as select * from iris limit 100;

-- conn = pool_mem
-- qname = t2
CREATE TABLE iris2 as select * from iris1 limit 50;

-- conn = pool_mem
-- qname = t3
CREATE TABLE iris3 as select * from iris2 limit 10;
