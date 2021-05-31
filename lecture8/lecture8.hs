data Student = Student String String Int

getAverage :: [Student] -> Int
getAverage l = (foldr (\(Student a b c) acc -> c + acc) 0 l) `div` (length l)



