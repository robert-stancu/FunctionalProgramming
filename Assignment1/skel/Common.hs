module Common where

import Data.List

vflip :: [[a]] -> [[a]]
vflip = reverse

myFlip :: [[a]] -> [[a]]
myFlip = hflip . hflip

hflip :: [[a]] -> [[a]] 
hflip = map reverse

rot90r :: [[a]] -> [[a]]
rot90r = map reverse . transpose

rot90l :: [[a]] -> [[a]]
rot90l = reverse . transpose

rot180l :: [[a]] -> [[a]]
rot180l = rot90l . rot90l

rot180r :: [[a]] -> [[a]]
rot180r = rot90r . rot90r



