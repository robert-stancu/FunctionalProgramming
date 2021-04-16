import Data.Char

rem_upper l = map (\s -> map toLower s) l

longer :: Int -> [String] -> [String]
longer x = filter (\s -> length s < x) 

howMany l = foldr (\x accum -> if length x > 12 then (accum + 1) else accum) 0 l

split sep s = foldr (\c (x:xs) ->
       if c == sep
       then "":x:xs
       else (c:x):xs ) [[]] s

names_emails :: [String] -> [[String]]
names_emails l = map (split '@') l

removeDuplicates (x:xs) = x : removeDuplicates (filter (/= x) xs)
removeDuplicates [] = []

domains :: [String] -> [String]
domains l = foldr (\x accum -> accum ++ tail (split '@' x)) [] l 
