

module Eleicoes(Candidato(..),
                Estado(..),
                Nacao,
                criaNacao,
                obterEstado,
                adicionaVotosEstado,
                adicionaVotosNacao,
                vencedorEstado,
                vencedorEleicao
                )where

---1
data Candidato = A | B deriving (Eq,Show)

---sintaxe de registo
data Estado = Estado{ nome :: String,
                      peso :: Int,
                      votosA :: Int,
                      votosB :: Int  }

---sinonimo de tipos
type Nacao = [Estado]

---2
criaNacao :: [(String,Int)] -> Nacao
criaNacao [] = []
criaNacao [(c,v)] = Estado c v 0 0 :[]
criaNacao (x:y:xs) = Estado ((\(a,b)->a) x) ((\(a,b)->b) x) 0 0 : criaNacao (y:xs)

---3
obterEstado :: Nacao -> String -> Estado
obterEstado xs nomeEstado = head $ filter(\(Estado x _ _ _ ) -> x==nomeEstado) xs

---4
adicionaVotosEstado :: Estado -> Int -> Int -> Estado
adicionaVotosEstado estado votosa votosb = head $ map (\(Estado a b x y) -> Estado a b (x+votosa) (y+votosb)) $ estado:[]

---5
adicionaVotosNacao :: Nacao -> [(String,Int,Int)] -> Nacao
adicionaVotosNacao [] [] = []
adicionaVotosNacao [x] [] = [x]
adicionaVotosNacao [] [x] = []
adicionaVotosNacao [(Estado s d f g)] [(nome,votosA,votosB)] = [Estado s d (f+votosA) (g+votosB)]
adicionaVotosNacao ((Estado s d f g):ls) ((nome,votosA,votosB):xs)
      | s == nome     = (Estado s d aux aux2) : adicionaVotosNacao (ls) (xs)
      | otherwise = adicionaVotosNacao [(Estado s d f g)] xs ++ adicionaVotosNacao ls ((nome,votosA,votosB):xs)
    where aux = head $ map(+f) (votosA:[])
          aux2 = head $ map (+g) (votosB:[])

---6
vencedorEstado :: Estado -> Maybe Candidato
vencedorEstado (Estado x y s d)
            | s > d = Just A
            | s < d = Just B
            |otherwise = Nothing

---7
vencedorEleicao :: Nacao -> Maybe Candidato
vencedorEleicao nacao
              | contaVotosPelasLetras (comparaVotos nacao) 'a' > contaVotosPelasLetras (comparaVotos nacao) 'b' = Just A
              | contaVotosPelasLetras (comparaVotos nacao) 'a' < contaVotosPelasLetras (comparaVotos nacao) 'b' = Just B
              | otherwise = Nothing

---MetodoAux para contar os votos;se votosA maior que VotosB adiciono 'a';se votosB maior que votosA adiciono 'b'; 'e' caso empatem
comparaVotos :: Nacao -> [Char]
comparaVotos nacao =  map(\(Estado x s f g)  -> if f > g then 'a' else (if f<g then 'b' else 'e')) nacao

---MetodoAux para contar o numero de votos atravez do numero de ocurrencias de 'a','b','e'
contaVotosPelasLetras :: Eq a => [a] -> a -> Int
contaVotosPelasLetras votos c = length $ filter (== c) votos

---8
instance Eq Estado where
  Estado s c b d == Estado f g h j = c==g
   && vencedorEstado (Estado s c b d) == vencedorEstado(Estado f g h j)

---9
instance Show Estado where
  show (Estado x c v b) = x++" "++ show(c) ++ " " ++ show(v) ++ " " ++ show(b)
