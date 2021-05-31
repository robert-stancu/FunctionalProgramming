module Greyscale where

type Pixel = Int
type Image = [[Pixel]]

invert :: Image -> Image
invert = map (map (\x -> 255 - x))

crop :: (Int, Int, Int, Int) -> Image -> Image
crop = (\(a, b, c, d) img -> map (\line -> drop (b - d) (take b line) ) (take c (drop (a-1) img)) )

brightness :: Int -> Image -> Image
brightness = (\x -> map (map (\value -> if value + x > 255 then 255 else value + x)))

mask :: [[Bool]] -> Image -> Image
mask = zipWith (zipWith (\a b -> if a == False then 0 else b))
