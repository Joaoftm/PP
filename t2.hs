

---A.paresConsecutivos/diferencasConsecutivas
---1

paresConsecutivos :: [a] -> [(a, a)]
paresConsecutivos [] = []
paresConsecutivos [_] = []
paresConsecutivos [ primeiro,segundo ] = [( primeiro,segundo )]
paresConsecutivos (primeiro:segundo:ls) =
                              (primeiro,segundo) : paresConsecutivos (segundo:ls)

---2

diferencasConsecutivas :: [Int]->[Int]
diferencasConsecutivas [] = []
diferencasConsecutivas [_] = []
diferencasConsecutivas [x,y] = [y-x]
diferencasConsecutivas (x:y:xs) = y-x : diferencasConsecutivas (y:xs)


---B.Jogadores de futebol
---1

claramentePior:: (String,Int,Int) -> [(String,Int,Int)] -> Bool
claramentePior _ [] = True
claramentePior (x,y,z) ((o,c,v):ys) =  y + z < c+v  && (claramentePior (x,y,z) ys)

---2


filtroAux :: (Ord c, Num c) => [(String, c, c)] -> [(String, c, c)]
filtroAux [] = []
filtroAux [_] = []
filtroAux  ((o,c,v):(d,f,g):ys) = if c+v >= f+g then [(o,c,v)] ++
                  filtroAux ((d,f,g):ys )
                  else [(d,f,g)] ++ filtroAux  (ys)

filtroJogadores :: (Ord c, Num c) => [(String, c, c)] -> [(String, c, c)]
filtroJogadores [] = []
filtroJogadores [x] = [x]
filtroJogadores (x:y:xs) = filtroAux (x:y:xs)


---C.Preencher o buraco

preencherVazio :: [[Int]] -> Int
preencherVazio (linha:ls)
    |null falta = preencherVazio ls
    |otherwise = head falta
    where
    falta = faltaAUX [1..9] linha
    remove x (y:ys)
      | x == y = ys
      | otherwise = y : (remove x ys)
    remove x [] = []
    faltaAUX l [] = l
    faltaAUX l (0:xs) = faltaAUX l xs
    faltaAUX l (x:xs) = faltaAUX (remove x l) xs
