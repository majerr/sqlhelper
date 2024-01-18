-- execmethod = sendq
-- qname = binding_example
SELECT species, [Petal.Width]
FROM IRIS
WHERE SPECIES = ? AND
[Petal.Width] < {petal_width};

