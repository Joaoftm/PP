import System.IO
import System.Environment
import Data.List
import Test.QuickCheck

main :: IO ()
main = do
    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering
    flag <- getArgs
    if flag == ["-t"] then
		do
		quickCheck prop_cols
		quickCheck prop_filterIn
		quickCheck prop_tupList
    else
		do
		functions <- getLine
		if functions == "exit"
		then
			return ()
		else
			do
			input <- getLine
			if input == "exit" then return () else funcMain [input] [] (tupList (words functions))

funcMain :: [String] -> [Float] -> [([Char], Int)] -> IO ()
funcMain inputs res funcs = let result = (applyFuncs inputs res funcs) in do
    putStrLn (show result)
    input <- getLine
    if input == "exit" then return () else funcMain (input:inputs) (result:res) funcs


applyFuncs :: [String] -> [Float] -> [([Char], Int)] -> Float
applyFuncs (input:inputs) [] ((func,col):[]) = getCol (words input) col
applyFuncs (input:inputs) [] ((func,col):groups) = getCol (words input) col
applyFuncs (input:inputs) res ((func,col):[])
    | func == "sum" = (head res) + (getCol (words input) col)
    | func == "average" = avg (filterInputs input (input:inputs) col [])
    | func == "maximum" = max (head res) (getCol (words input) col)
applyFuncs (input:inputs) res ((func,col):groups)
    | func == "sum" = sum (filterInputs input (input:inputs) col (cols groups))
    | func == "average" = avg (filterInputs input (input:inputs) col (cols groups))
    | func == "maximum" = maximum (filterInputs input (input:inputs) col (cols groups))

getCol :: [String] -> Int -> Float
getCol xs i = read (head (drop i xs)) :: Float

cols :: [([Char], Int)] -> [Int]
cols [] = []
cols ((_,col):xs) = (col:(cols xs))

avg :: [Float] -> Float
avg xs = ((sum xs)/(fromIntegral (length xs)))

--requires format "metric col (optional->) groupby col groupby col ..."
tupList :: [String] -> [([Char], Int)]
tupList [] = []
tupList (x:y:xs) = ((x, (read y :: Int)):(tupList xs))

filterInputs :: [Char] -> [String] -> Int -> [Int] -> [Float]
filterInputs _ [] _ _ = []


filterInputs input (x:xs) col cols
    | (filterIn (words input) cols 0) == (filterIn (words x) cols 0) = ((getCol (words x) col):(filterInputs input xs col cols))
    | otherwise = (filterInputs input xs col cols)

filterIn :: [String] -> [Int] -> Int -> [String]
filterIn [] _ _ = []
filterIn (x:xs) ys i = if elem i ys then (x:(filterIn xs ys (i+1))) else (filterIn xs ys (i+1))


-------
containsUnord :: [String] -> [String] -> Bool
containsUnord ys [] = True
containsUnord ys (x:xs) = if elem x ys then containsUnord ys xs else False

maximo :: [Int] -> Int
maximo [] = 0
maximo [x] = x
maximo (x:y:xs) = maximo ((if x >= y then x else y):xs)


prop_cols :: [([Char], Int)] -> Bool
prop_cols xs = (length xs) == (length (cols xs))

prop_filterIn :: [String] -> [Int] -> Property
prop_filterIn xs ys = (maximo ys) < (length xs) && (length ys) <= (length xs) ==> containsUnord xs (filterIn xs ys 0)

prop_tupList :: [String] -> Property
prop_tupList xs = ((mod (length xs) 2) == 0) ==> (length xs) == (length (tupList xs))*2
