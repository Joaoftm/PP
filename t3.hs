

----A. Listas de Alguidares
--- 1
listaDeAlguidaresVazia :: [a]
listaDeAlguidaresVazia = []

--- 2
adicionaAListaDeAlguidares :: (Ord a) => Int -> a -> [[a]] -> [[a]]
adicionaAListaDeAlguidares n add [] = [[add]]
adicionaAListaDeAlguidares n add (x:xs)
  | length x < n = adicionaAUX add x : xs
  | add < last (x)  = adicionaAUX add (init x) : adicionaAListaDeAlguidares n (last x) xs
  | otherwise = x : adicionaAListaDeAlguidares n add xs

adicionaAUX :: (Ord a) => a -> [a] -> [a]
adicionaAUX add [] = [add]
adicionaAUX add (x:xs)
    | x < add = x : adicionaAUX add xs
    | otherwise = add : x : xs

--- 3
---minhaPrimeiraAssinatura:elemListaDeAlguidares :: (Foldable t, Eq a) => a -> [t a] -> Bool

elemListaDeAlguidares :: Ord a => a -> [[a]] -> Bool
elemListaDeAlguidares a xs = or (map (elem a) xs)

--- 4
---minhaPrimeiraAssinatura:removerDaListaDeAlguidares :: Eq a => a -> [[a]] -> [[a]]

removerDaListaDeAlguidares :: Ord a => a -> [[a]] -> [[a]]
removerDaListaDeAlguidares a xs = map (filter (/=a)) xs


--- 5
---minhaPrimeiraAssinatura:fromList :: (Foldable t, Ord a) => Int -> t a -> [[a]]

fromList :: Ord a => Int -> [a] -> [[a]]
fromList size xs = foldr (adicionaAListaDeAlguidares size) [] xs

--- 6
---minhaPrimeiraAssinatura:mapListaDeAlguidares::(Ord a,Ord b ) => Int -> (a->b) -> [[a]] -> [[b]]

mapListaDeAlguidares :: (Ord a, Ord b) => Int -> (a -> b) -> [[a]] -> [[b]]
mapListaDeAlguidares n func xs = fromList n $ concat (map (map func) xs)

----B.Key-Value Store
---1
---minhaPrimeiraAssinatura:createFastCache :: Int -> [a] -> [b] -> [[(a, b)]]

createFastCache :: (Ord a, Ord b) => Int -> [a] -> [b] -> [[(a, b)]]
createFastCache n xs ys = separaAUX n $ combinaAUX xs ys

combinaAUX :: [a] -> [b] -> [(a, b)]
combinaAUX [] _  = []
combinaAUX _ [] = []
combinaAUX (x:xs) (y:ys) = (x, y) : combinaAUX xs ys

separaAUX :: Int -> [a] -> [[a]]
separaAUX x [] = []
separaAUX x s = take x s : separaAUX x (drop x s)

---2
---foldl dava ao contario "cb"
fastGet :: (Ord a, Ord b) => [[(a, b)]] -> a -> [b]
fastGet xs v =  foldr (\ (x,y) acc -> if v == x then y:acc else acc) [] $ concat xs
