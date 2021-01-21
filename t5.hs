

import System.Environment (getArgs)
import System.IO (hFlush,stdout)
import Data.List (isInfixOf,intersperse)

main :: IO()
main = do
    [args] <- getArgs
    ficheiroTxt <- readFile args
    loopCmds ficheiroTxt []

loopCmds :: String -> [String] -> IO()
loopCmds ficheiroTxt filters = do
   printTxt ficheiroTxt filters
   printFilters filters
   hFlush stdout
   cmd <- getLine
   case cmd of
     "pop" -> if null filters
                then putStr ""
                else loopCmds ficheiroTxt (pop filters)
     newFilter -> loopCmds ficheiroTxt (push newFilter filters)

---imprime o texto com o filtro
printTxt :: String -> [String] -> IO()
printTxt ficheiroTxt filters = putStr filtered
  where
    filtered = unlines $ filter containAUX txtLines
    txtLines = lines ficheiroTxt
    containAUX line = and $ map (`isInfixOf` line) filters

---imprime os filtros
printFilters :: [String] -> IO()
printFilters filters = do
     putStr "Filtering: "
     putStrLn $ concat (intersperse ", " filters)
     putStr "> "

---tira o/os filtros
pop :: [String] -> [String]
pop [] = []
pop xs = init xs

---adiciona novo/os filtros
push :: String -> [String] -> [String]
push x xs
   | elem x xs = xs
   | otherwise = xs ++ [x]
