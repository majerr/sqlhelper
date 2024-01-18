-- qname = how_many_irises
SELECT count(*) as N FROM IRIS;

-- qname = n_longest_setosa_petal_lengths
SELECT *
FROM (SELECT *
      FROM IRIS
      WHERE Species = 'setosa'
      ORDER BY [Petal.Length] DESC)
LIMIT {n_longest_petals}
