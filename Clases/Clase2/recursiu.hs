potencia :: Int -> Int -> Int
potencia x 0 = 1
potencia x y = x * potencia x (y - 1)

esPotencia :: Int -> Int -> Int -> Bool
esPotencia x b e
  | x == potencia b e = True
  | x < potencia b e = False
  | otherwise = esPotencia x b (e + 1)
