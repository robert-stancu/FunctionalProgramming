import Data.Char

numberOfElements :: [a] -> Integer
numberOfElements [] = 0
numberOfElements (x:xs) = 1 + numberOfElements xs

concatenate :: [[Integer]] -> [Integer]
concatenate [] = []
concatenate (x:xs) = x ++ concatenate xs 

concatenateWithCons :: [[Integer]] -> [Integer]
concatenateWithCons [] = []
concatenateWithCons ([]:xs) = concatenateWithCons xs
concatenateWithCons ((x:xs):xss) = x : concatenateWithCons (xs:xss)

isIn :: Eq a => a -> [a] -> Bool
isIn e [] = False
isIn e (x:xs)
    | e == x = True
    | otherwise = isIn e xs

removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates l = f [] l
    where f seen [] = seen
          f seen (x:xs)
            | isIn x seen == True = f seen xs
            | otherwise = f (seen ++ [x]) xs

concatenateTail :: [[Integer]] -> [Integer]
concatenateTail l = help [] l 
    where help acc [] = acc
          help acc (x:xs) = help (acc ++ x) xs

concatConsTail :: [[Integer]] -> [Integer]
concatConsTail l = help [] l 
    where help acc [] = acc
          help acc ([]:xs) = help acc xs
          help acc ((x:xs):xss) = help (x:acc) (xs:xss)

retAllDuplicates :: Eq a => [a] -> [a]
retAllDuplicates l = help [] [] l 
    where help seen dups [] = dups
          help seen dups (x:xs)
            | isIn x seen == True = help seen (dups++[x]) xs
            | otherwise = help (seen++[x]) dups xs

makeFirstUpper :: String -> String
makeFirstUpper [] = []
makeFirstUpper (x:[]) = (toUpper x) : []
makeFirstUpper (x:xs) = (toUpper x) : xs

makeWordsUpper :: [String] -> [String]
makeWordsUpper l = help [] l 
    where help acc [] = acc
          help acc (x:xs) = help ((makeFirstUpper x) : acc) xs

makeAllUpper :: String -> String
makeAllUpper l = help [] l 
    where help acc [] = acc
          help acc (x:xs) = help (acc ++ [toUpper x]) xs

allWordsUpperLetters :: [String] -> [String]
allWordsUpperLetters l = help [] l 
    where help acc [] = acc
          help acc (x:xs) = help (acc ++ [(makeAllUpper x)]) xs

allWordsUpperNonTail :: [String] -> [String]
allWordsUpperNonTail [] = []
allWordsUpperNonTail (x:xs) = (makeAllUpper x) : allWordsUpperNonTail xs

search :: String -> String -> Integer
search [] pattern = 0
search text [] = 1 + (search text pattern)
search (x:xs) (y:ys)
    | x == y = search xs ys 
    | otherwise = search xs (y:ys)

    