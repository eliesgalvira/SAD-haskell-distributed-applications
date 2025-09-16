sumaLlindarIf :: (Num a, Ord a) => a -> a -> a -> a
sumaLlindarIf x y llindar = if x + y < llindar 
    then x + y
    else llindar

sumaLlindarLetIn :: (Num a, Ord a) => a -> a -> a -> a
sumaLlindarLetIn x y llindar =
    let s = x + y
    in if s < llindar then s else llindar

sumaLlindarWhere :: (Num a, Ord a) => a -> a -> a -> a
sumaLlindarWhere x y llindar = if s < llindar then s else llindar
    where s = x + y

sumaLlindarGuarda :: (Num a, Ord a) => a -> a -> a -> a
sumaLlindarGuarda x y llindar
    | s < llindar = s
    | otherwise = llindar
    where s = x + y
