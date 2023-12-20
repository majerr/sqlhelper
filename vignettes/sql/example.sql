-- qname = how_many_irises
SELECT count(*) as N FROM IRIS;

-- qname = short-petal-setosa
select Species, `Petal.Length`
FROM IRIS
WHERE Species = "setosa"
AND `Petal.Length` < {petal_length}
