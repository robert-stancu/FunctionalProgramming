module ParseNetpbm where

import Parser
import Netpbm

import Control.Applicative
import Data.Char

-- staight outta Data.List.Split
-- https://hackage.haskell.org/package/split-0.2.3.4/docs/src/Data.List.Split.Internals.html#divvy
-- https://hackage.haskell.org/package/split-0.2.3.4/docs/src/Data.List.Split.Internals.html#chop
chop :: ([a] -> (b, [a])) -> [a] -> [b]
chop _ [] = []
chop f as = b : chop f as'
  where (b, as') = f as

divvy :: Int -> Int -> [a] -> [[a]]
divvy _ _ [] = []
divvy n m lst = filter (\ws -> n == length ws) choppedl
  where choppedl = chop (\xs -> (take n xs , drop m xs)) lst

netpbm :: Parser Netpbm
netpbm = pbm <|> pgm <|> ppm

infixl 4 <.*>
x <.*> y = (x <* ignore) <*> y

infixl 4 .*>
x .*> y = (x <* ignore) *> y

filterMaybe :: (a -> Bool) -> Maybe a -> Maybe a
filterMaybe p j@(Just x) | p x = j
filterMaybe _ _ = Nothing

header :: Char -> Parser Char
header v = token 'P' *> token v <* ws

pbm :: Parser Netpbm
pbm = header '1' .*> pbmcontents (\(w, h, px) -> PBM w h px) bool

pgm :: Parser Netpbm
pgm = header '2' .*> contents (\(w, h, m, px) -> PGM w h m px) int

ppm :: Parser Netpbm
ppm = header '3' .*> contents (\(w, h, m, px) -> PPM w h m px) rgb

normalizepx :: (Int, Int, Int, [a]) -> (Int, Int, Int, [[a]])
normalizepx (w, h, m, px) = (w, h, m, divvy w w px)

contents :: ((Int, Int, Int, [[a]]) -> Netpbm) -> Parser a -> Parser Netpbm
contents c p = c . normalizepx <$> temp p

temp :: Parser a -> Parser (Int, Int, Int, [a])
temp p = (,,,) <$> int <.*> int <.*> int <.*> ppixels p

pbmnormalizepx :: (Int, Int, [a]) -> (Int, Int, [[a]])
pbmnormalizepx (w, h, px) = (w, h, divvy w w px)

pbmcontents :: ((Int, Int, [[a]]) -> Netpbm) -> Parser a -> Parser Netpbm
pbmcontents c p = c . pbmnormalizepx <$> pbmtemp p

pbmtemp :: Parser a -> Parser (Int, Int, [a])
pbmtemp p = (,,) <$> int <.*> int <.*> ppixels p

ppixels :: Parser a -> Parser [a]
ppixels p = many $ p <* ws

ws :: Parser String
ws = many (space <|> nl)

nonNL :: Parser Char
nonNL = spot (/= '\n')

comment :: Parser String
comment = token '#' *> many nonNL <* nl

ignore :: Parser String
ignore = comment <|> ws

space :: Parser Char
space = token ' '

nl :: Parser Char
nl = token '\n'

int :: Parser Int
int = read <$> some (spot isDigit)

bool :: Parser Bool
bool = (== '1') <$> (token '0' <|> token '1')

rgb :: Parser (Int, Int, Int)
rgb = (,,) <$> int <.*> int <.*> int
