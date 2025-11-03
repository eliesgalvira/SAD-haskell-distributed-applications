sumAll :: Num a => [a] -> a
sumAll = foldr (+) 0

andAll :: [Bool] -> Bool
andAll = foldr (&&) True

orAll :: [Bool] -> Bool
orAll = foldr (||) False

-- Add custom function
-- foldr (\elem acc -> <term>) <start_acc> <list>

count e = foldr (\x acc -> if e==x then acc+1 else acc) 0

isAll e = foldr (\x acc -> e==x && acc) True -- (\x -> (&&) $ e==x) is equivalent


lengthr :: Num n => [a] -> n
lengthr = foldr (\x -> (+) 1) 0 -- (\x acc -> 1 + acc) is equivalent

mapr f = foldr ((:) . f) [] -- (\x acc -> f x : acc)

mapl f = foldl (\acc x -> f x : acc) []
