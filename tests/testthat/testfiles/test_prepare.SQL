-- qname = selectn
SELECT {n};

-- qname = selectfoo
SELECT {`foo`};

-- qname = nointerpolate
-- interpolate = no
SELECT {`foo`};

-- interpolate = yes
-- qname = noquote
-- quotesql = no
-- execmethod = execute
SELECT {`foo`};

-- qname = poolcon
-- conn_name = pool_mem
-- execmethod = spatial
-- geometry = geom
select 'foo';
