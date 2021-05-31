{-
     [[1,2,3],[4,5,6],[7,8,9]]
     ["1 2 3 \n", "4 5 6 \n", "7 8 9 \n"]

     "1 2 3 \n4 5 6\n7 8 9 "
-}

bind :: Char -> [String] -> String
bind c l = foldr (\s acc -> s ++ [c] ++ acc) [] l

displayMatrix :: [[Integer]] -> String
--displayMatrix m = bind '\n' (map (bind ' ')(map (map show) m))

displayMatrix = ((bind '\n') .
                (map ((bind ' ').(map show))))

transpose ([]:_) = []
transpose m = (map head m) : (transpose (map tail m))

value li cj = foldr (+) 0 (zipWith (*) li cj)
line li trm2 = map (\c -> value li c) trm2
productMatrix m1 trm2 = map (\l -> line l trm2) m1


