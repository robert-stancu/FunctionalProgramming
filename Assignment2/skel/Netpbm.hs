module Netpbm where

data Netpbm = PGM Int Int Int [[Int]] |
              PBM Int Int [[Bool]] |
              PPM Int Int Int [[(Int, Int, Int)]]


instance Show Netpbm where
    show (PBM w h p) = "P1" ++ "\n" ++ show w ++ " " ++ show h ++ "\n" ++ pxsToString p
        where pxsToString = foldr (\x z -> lineToString x ++ "\n" ++ z) ""
              lineToString = foldr (\x z -> (if x then "1" else "0") ++ " " ++ z) ""
    show (PGM w h m p) = "P2" ++ "\n" ++ show w ++ " " ++ show h ++ "\n" ++
        show m ++ "\n" ++ pxsToString p
        where pxsToString = foldr (\x z -> lineToString x ++ "\n" ++ z) ""
              lineToString = foldr (\x z -> show x ++ " " ++ z) ""
    show (PPM w h m p) = "P3" ++ "\n" ++ show w ++ " " ++ show h ++ "\n" ++
        show m ++ "\n" ++ pxsToString p
        where pxsToString = foldr (\x z -> lineToString x ++ "\n" ++ z) ""
              lineToString = foldr (\(r, g, b) z -> show r ++ " " ++ show g ++ " " ++ show b ++ " "++ z) ""
