split sep s = foldr (\c (x:xs) ->
       if c == sep
       then "":x:xs
       else (c:x):xs ) [[]] s

get_resource :: String -> String
get_resource = (\l -> (split '/' l) !! 1)

{- domain-name.com/books/get_item?param1=1234&param2=5678
   ["domain-name.com/books/get_item?param1=1234&param2=5678", "gmail.com/books/get_item?param1=1234&param2=5678", "gmail.com/books/get_item?param1=1234&param2=5678&param3=000"] -}

get_action :: String -> String
get_action = (\l -> split '/' (split '?' l !! 0) !! 2)

get_params :: String -> [String]
get_params = (\l -> foldr (\x accum -> tail (split '=' x) ++ accum) [] (split '&' (head (tail (split '?' l)))))

get_domain :: String -> String
get_domain = (\l -> head (split '/' l))

filterURLs :: [String] -> [String]
filterURLs = (\l ->
    filter (\x -> get_action x == "get_item" || get_action x == "remove_item" || get_action x == "insert_item")
    (filter (\x -> length (get_params x) > 2) 
    (filter (\x -> get_resource x == "books" || get_resource x == "movies")  
    (filter (\x -> get_domain x == "gmail.com" || get_domain x == "yahoo.com") l))))

evenSet :: Integer -> Bool
evenSet x = mod x 2 == 0

mem :: (Integer -> Bool) -> Integer -> Bool
mem = (\s val -> s val)

powerSet :: Integer -> Bool
powerSet 2 = True
powerSet x = mod x 2 == 0 && (powerSet $ div x 2)  

naturalSet x = x == fromInteger (round x)

intersection :: (Integer -> Bool) -> (Integer -> Bool) -> (Integer -> Bool)
intersection = (\x y z -> x z && y z)