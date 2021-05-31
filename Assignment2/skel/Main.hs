module Main where

import Data.Maybe
import Control.Applicative
import Netpbm
import NetpbmProcess
import ParseNetpbm
import Parser

import System.Environment

-- !!!!! To run your desired function edit this line...
myTransform img = scale (3, 0.4) img

-- ... or this one
myTransform2 img1 img2 = stamp (100, 100) (0.2, 0.2) img1 img2


serializeTo f = writeFile f . maybe "" show

applyTransform :: String -> Maybe Netpbm
applyTransform = fmap myTransform <$> parse netpbm

applyTransform2 :: String -> String -> Maybe Netpbm
applyTransform2 s1 s2 = liftA2 myTransform2 img1 img2
    where img1 = parse netpbm s1
          img2 = parse netpbm s2

processArgs :: [String] -> IO ()
processArgs args
    | length args == 2 = do
        let outf = head . tail $ args
        readFile inf >>= serializeTo outf . applyTransform
    | length args == 3 = do
        let maskf = head . tail $ args
        let outf = head . tail . tail $ args
        mapM readFile [inf, maskf] >>= (\[i1, i2] -> serializeTo outf $ applyTransform2 i1 i2)
    where inf = head args
          pbmparse = parse netpbm


main :: IO ()
main = do
    args <- getArgs
    processArgs args
