module Greyscale where

import Data.List
import Common

type Pixel = Int
type Image = [[Pixel]]

invert :: Image -> Image
invert = undefined

crop :: (Int, Int, Int, Int) -> Image -> Image
crop = undefined

brightness :: Int -> Image -> Image
brightness = undefined

mask :: [[Bool]] -> Image -> Image
mask = undefined

superimpose :: Float -> Image -> Image -> Image
superimpose alpha i1 i2 = undefined

contrast :: Float -> Image -> Image
contrast lvl img = undefined

border :: Int -> Image -> Image
border thickness img = undefined

blur :: Image -> Image
blur img = undefined
