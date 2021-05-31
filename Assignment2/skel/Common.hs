module Common where

vflip :: [[a]] -> [[a]]
vflip = undefined

hflip :: [[a]] -> [[a]]
hflip = undefined

rot90r :: [[a]] -> [[a]]
rot90r = undefined

rot90l :: [[a]] -> [[a]]
rot90l = undefined

rot180l :: [[a]] -> [[a]]
rot180l = undefined

rot180r :: [[a]] -> [[a]]
rot180r = undefined

scale :: (Float, Float) -> [[a]] -> [[a]]
scale (vf, hf) img = undefined

stamp :: (Int, Int) -> (Float, Float) -> [[a]] -> [[a]] -> [[a]]
stamp (y, x) (vf, hf) target st = undefined
