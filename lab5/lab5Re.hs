import Data.Char

rem_upper :: [String] -> [String]
rem_upper l = map (map toLower ) l

longer :: Int -> [String] -> [String]
longer x l = filter (\e -> length e <= x) l

