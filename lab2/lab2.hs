myAnd :: Bool -> Bool -> Bool
myAnd True True = True
myAnd False False = True
myAnd True False = False
myAnd False True = False

ifp :: Bool -> a -> a -> a
ifp True x y = x 
ifp False x y = y

max3 :: Integer -> Integer -> Integer -> Integer
max3 a b c = if (a > b && a > c) then a else
            if (b > a && b > c) then b else c

max3guards :: Integer -> Integer -> Integer -> Integer
max3guards a b c
    | (a > b && a > c) = a 
    | (b > a && b > c) = b 
    | otherwise = c

reversal :: [a] -> [a]
reversal [] = []
reversal l = reversal (tail l) ++ [head l]

lengthOfList :: [a] -> Integer
lengthOfList [] = 0
lengthOfList l = 1 + lengthOfList (tail l)

thirdToLast :: [Integer] -> Bool
thirdToLast l 
    | lengthOfList l < 3 = False
    | (head (tail (tail (reversal l))) `mod` 2) == 0 = False
    | otherwise = True

thirdToLastPatterns :: [Integer] -> Bool
thirdToLastPatterns xs 
    | (y `mod` 2 == 0) = False
    | otherwise = True
    where (x:y:z:xss) = reversal xs

sumOfIntegers :: [Integer] -> Integer
sumOfIntegers [] = 0
sumOfIntegers (x:xs) = x + sumOfIntegers xs

atLeastOneFalse :: [Bool] -> Bool
atLeastOneFalse [] = True
atLeastOneFalse (x:xs)
    | x == False = False
    | otherwise = atLeastOneFalse xs

filterOdds :: [Integer] -> [Integer]
filterOdds [] = []
filterOdds (x:xs)
    | x `mod` 2 == 0 = x : filterOdds xs
    | otherwise = filterOdds xs

intsToBools :: [Bool] -> [Integer]
intsToBools [] = []
intsToBools (x:xs)
    | x == True = 1 : intsToBools xs
    | otherwise = 0 : intsToBools xs

removeEmptyStrings :: [String] -> [String]
removeEmptyStrings [] = []
removeEmptyStrings (x:xs)
    | lengthOfList x > 0 = x : removeEmptyStrings xs
    | otherwise = removeEmptyStrings xs

removeSmallerThan3 :: [String] -> [String]
removeSmallerThan3 [] = []
removeSmallerThan3 (x:xs)
    | lengthOfList x >= 3 = x : removeSmallerThan3 xs
    | otherwise = removeSmallerThan3 xs

removeLetterA :: [String] -> [String]
removeLetterA [] = []
removeLetterA (x:xs)
    | lengthOfList x < 3 = x : removeLetterA xs
    | head (tail (tail x)) == 'a' = removeLetterA xs
    | otherwise = x : removeLetterA xs

onlyNames :: [String] -> [String]
onlyNames [] = []
onlyNames (x:xs)
    | head x >= 'A' && head x <= 'Z' = x : onlyNames xs
    | otherwise = onlyNames xs

removeLastName :: String -> String
removeLastName l = head l : f (tail l)
    where f [] = []
          f (x : xs) = if x >= 'A' && x <= 'Z' then [] else x : (f xs)

removeLastNames :: [String] -> [String]
removeLastNames [] = []
removeLastNames l = removeLastName x : removeLastNames xs
    where (x:xs) = onlyNames l