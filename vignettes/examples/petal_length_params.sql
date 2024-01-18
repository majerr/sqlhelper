-- qname = short_petal_setosa
select Species, `Petal.Length`
FROM IRIS
WHERE Species = "setosa"
AND `Petal.Length` < {petal_length};

-- qname = setosa_petal_1sd_below
select Species, `Petal.Length`
FROM IRIS
WHERE Species = "setosa"
AND `Petal.Length` < {
  mean(iris$Petal.Length[iris$Species == "setosa"])
    -  sd(iris$Petal.Length[iris$Species == "setosa"])
}
