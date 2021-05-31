import Data.Char

data TFU = TrueVal | FalseVal | Unknown deriving Show

myAnd :: TFU -> TFU -> TFU
myAnd TrueVal FalseVal = FalseVal
myAnd TrueVal TrueVal = TrueVal
myAnd TrueVal Unknown = Unknown
myAnd FalseVal FalseVal = FalseVal
myAnd FalseVal TrueVal = FalseVal
myAnd FalseVal Unknown = Unknown
myAnd Unknown TrueVal = Unknown
myAnd Unknown FalseVal = Unknown
myAnd Unknown Unknown = Unknown

data Nat = SpecialNat | Successor Nat deriving Show

natToInteger :: Nat -> Integer
natToInteger SpecialNat = 0
natToInteger (Successor a) = 1 + natToInteger a

natFromInteger :: Integer -> Nat
natFromInteger 0 = SpecialNat
natFromInteger a = (Successor (natFromInteger (a-1)))

data Gender = Male | Female deriving (Eq, Show)
data ID = CNP String | Description String Integer Gender deriving Show

genderOfX :: ID -> Gender
genderOfX (CNP s) = if (s !! 0 == '1') || (s !! 0 == '5') then Male else Female
genderOfX (Description _ _ c) = c

getGender :: Gender -> [ID] -> [ID]
getGender g = filter (\x -> genderOfX x == g)

yearOfX :: ID -> Integer
yearOfX (CNP s) = if (s !! 0 == '1') || (s !! 0 == '2') then 1900 + 
    (toInteger(digitToInt (s !! 5)) * 10 + toInteger(digitToInt (s !! 6))) 
        else 2000 + 
            (toInteger(digitToInt (s !! 5)) * 10 + toInteger(digitToInt (s !! 6)))
yearOfX (Description _ year _) = year 

getYear :: Integer -> [ID] -> [ID]
getYear y = filter (\x -> yearOfX x == y)
    
data Range = RangeOneGender Integer Integer Gender | RangeAllGenders Integer Integer

getRange :: Range -> [ID] -> [ID]
getRange (RangeOneGender start end gender) = filter (\x -> (yearOfX x >= start &&
                                                        yearOfX x <= end) && 
                                                        genderOfX x == gender)
getRange (RangeAllGenders start end) = filter (\x -> yearOfX x >= start &&
                                                        yearOfX x <= end) 

data List a = Null | Cons a (List a) deriving Show

size :: List a -> Integer
size Null = 0
size (Cons _ x) = 1 + size x

helper :: List a -> List a -> List a
helper (Cons x xs) l = helper xs (Cons x l)
helper Null l = l 

list = (Cons 1 (Cons 2 (Null)))

myreverse :: List a -> List a 
myreverse = (flip helper) Null 

toHaskellList :: List a -> [a]
toHaskellList Null = []
toHaskellList (Cons x xs) = [x] ++ toHaskellList xs 

fromHaskellList :: [a] -> List a 
fromHaskellList [] = Null
fromHaskellList (x:xs) = (Cons x (fromHaskellList xs))


