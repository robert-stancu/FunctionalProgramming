data FTree = Dir String [FTree] | File String deriving (Show, Eq)

t = Dir "/" [Dir "usr" [Dir "share" [],
                        Dir "var" [],
                        Dir "include" [File "stdio.h", File "string.h", File "stdlib.h"]],
             Dir "dev" [File "sda", File "sda1", File "sda2"],
             Dir "etc" [File "sudoers", File "passwd", File "shadow"],
             Dir "home" [Dir "mihai" [File ".zshrc", Dir "sol" [File "Lab10.hs"]],
                         Dir "student" [File ".bashrc", Dir "sol" []]]]

type Path = [String]
 
path1 = ["/", "usr", "include", "stdio.h"]
path2 = ["/", "home", "student"]
wrongPath1 = ["/", "usr", "include", "math.h"]
wrongPath2 = ["/", "sbin"]

dfs :: FTree -> [String]
dfs (File fname) = [fname]
dfs (Dir dname children) = dname : foldr (++) [] (map dfs children)

countFiles :: FTree -> Int
countFiles (File fname) = 1
countFiles (Dir _ children) = foldr (+) 0 (map countFiles children)

maxList :: [Int] -> Int
maxList l = helper l (-1)
    where helper [] myMax = myMax
          helper (x:xs) myMax = if x > myMax then helper xs x else helper xs myMax

longestPath :: FTree -> Int
longestPath (File _) = 1
longestPath (Dir _ children) =  maxList (map (\x -> longestPath x + 1) children)

findEntry :: String -> FTree -> Bool
findEntry s root = elem s (dfs root)

checkExists :: Path -> FTree -> Bool
