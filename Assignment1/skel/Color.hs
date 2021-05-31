module Color where

type Pixel = (Int, Int, Int)
type Image = [[Pixel]]

invert :: Image -> Image
invert = map (map (\(a, b, c) -> (255-a, 255-b, 255-c)))

crop :: (Int, Int, Int, Int) -> Image -> Image
crop = (\(a, b, c, d) img -> map (\line -> drop (b - d) (take b line) ) (take c (drop (a-1) img)) )

swapColors :: Image -> Image
swapColors = map (map (\(a, b, c) -> (c, a, b)))

brightness :: Int -> Image -> Image
brightness = (\x -> map (map (\(a, b, c) -> (a+x, b+x, c+x))))

mask :: [[Bool]] -> Image -> Image
mask = zipWith (zipWith (\a (b, c, d) -> if a == False then (0, 0, 0) else (b, c, d)))

toGreyscale :: Image -> [[Int]]
toGreyscale = map (map (\(a, b, c) -> (a + b + c) `div` 3))
