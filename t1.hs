

---A.Detecção de Plágio

listSplit :: [a] -> [[a]]
listSplit ls = [ s:[] |s <- ls]

countNumbers :: Eq a => [a] -> [a] -> Int
countNumbers c string = length[ x | x <- c, y <- string, x==y]

frequencias :: Eq a => [a] -> [Int]
frequencias str = take (length str) [ c | c <- str,
                  y <- listSplit str, let c = countNumbers y str ]

---B.Pequenas Palavras

pequenasPalavras :: [[Char]]
pequenasPalavras = [ [x,y,z] | x <- "abcdefghijklmnopqrstuvwxyz",
      y <- "abcdefghijklmnopqrstuvwxyz", z <- "abcdefghijklmnopqrstuvwxyz",
      x `elem` ['a','e','i','o','u','y'] || y `elem` ['a','e','i','o','u','y']||
      z `elem` ['a','e','i','o','u','y'] ]


---C.Campainhas de Prédios

legendaCampainha :: (Num a, Enum a, Ord a, Show a) => a -> a -> [([Char], a)] -> [[Char]]
legendaCampainha andar salto ls = [ ( show ( if i < salto then i else i+1 )) ++ a | i <- [1..andar], (a,s) <- ls, i <= s]
