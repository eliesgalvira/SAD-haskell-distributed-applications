-- 2 Funcions
primer :: a -> b -> a
primer x _ = x

segon :: a -> b -> b
segon _ y = y

-- 2.1 Lambda
primerL :: a -> b -> a
primerL = \x -> \_ -> x

segonL :: a -> b -> b 
segonL = \_ -> \y -> y

-- 2.3 Prioritat / Associativitat
r1 = primer (segon 1 2) 3
r2 = segon (primer 1 2) 3
r3 = primer (1 + segon 1 2 + 1) 2
r4 = primer segon 1 2 3

-- 2.4 Notació infixa
(?) = primer
(??) = segon

r1i = 1 ?? 2 ? 3
r2i = 1 ? 2 ?? 3 
r3i = 1 + 1 ?? 2 + 1 ? 2
r4i = 2 ?? 3 ? 1

-- 2.5 Funció d'ordre superior
inter :: (a -> b -> c) -> b -> a -> c
inter f x y = f y x


segonI :: a -> b -> b
segonI x y = primer y x


e1 = (+) (inter (-) 1 2) (-1)
e2 = primer (primer (primer 0 0) 0) 0
e3 = 0 + primer (segon 1 0) 1
e3' = 0 + primer segon 1 0 1
e4 = div e1 e3'

-- 2.6 Aplicació parcial
equacioRecta :: Num a => a -> a -> a -> a
equacioRecta m b x = m * x + b

equacioRecta4 :: Num a => a -> a -> a
equacioRecta4 = equacioRecta 4
