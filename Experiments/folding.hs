sumAll :: Num a => [a] -> a
sumAll = foldr (+) 0

andAll :: [Bool] -> Bool
andAll = foldr (&&) True

orAll :: [Bool] -> Bool
orAll = foldr (||) False
