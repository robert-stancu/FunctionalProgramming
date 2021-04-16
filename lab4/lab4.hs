add4 :: [Integer] -> [Integer]
add4 [] = []
add4 l = map (+4) l

addSpace :: [String] -> [String]
addSpace [] = []
addSpace l = map (' ':) l

takeFirstK :: Eq a => Num a => a -> String -> String
takeFirstK k [] = []
takeFirstK 0 l = []
takeFirstK k (x:xs) = x : takeFirstK (k-1) xs

firstKcharacters :: Eq a => Num a => a -> [String] -> [String]
firstKcharacters k [] = []
firstKcharacters k l = map (takeFirstK k) l

removeEmptyStrings :: [String] -> [String]
removeEmptyStrings [] = []
removeEmptyStrings l = filter (/="") l

startWithCapital :: [String] -> [String]
startWithCapital [] = []
startWithCapital l = filter (\x -> head x >= 'A' && head x <= 'Z') l

productOfList :: [Integer] -> Integer
productOfList [] = 0
productOfList l = foldr (*) 1 l

maxOfList :: [Integer] -> Integer
maxOfList [] = error "no empty lists"
maxOfList l = foldr (\accum x -> if x > accum then x else accum) (-1) l

--["robi", "ceva"] -> "robi, ceva"

concatStrings :: [String] -> String
concatStrings [] = []
concatStrings l = foldr (++) [] l

isInList :: Eq a => a -> [a] -> Bool
isInList e [] = False
isInList e l = foldl (\accum x -> accum || (x == e)) False l

reversal :: [a] -> [a]
reversal [] = []
reversal l = foldl (\accum x -> x : accum) [] l