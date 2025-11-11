data Llista a = Buida | N a (Llista a)

primeraLlista :: Llista Char
primeraLlista = N 'a' (N 'b' Buida)

buida :: Llista a -> Bool
buida Buida = True
buida _ = False

afegir :: a -> Llista a -> Llista a
afegir valor llista = N valor llista

numElems :: Llista a -> Int
numElems Buida = 0
numElems (N _ xs) = 1 + numElems xs

-- Amb la llista de haskell
-- LLista no buida
primer :: [a] -> a
primer (x:_) = x

ultim :: [a] -> a
ultim [x] = x
ultim (x:xs) = ultim xs

-- take (toma els n primers)
primersN :: Int -> [a] -> [a]
primersN 0 _ = []
primersN _ [] = []
primersN n (x:xs) = x : primersN (n - 1) xs

--drop (treu els n primers)
treureN :: Int -> [a] -> [a]
treureN n xs = xs
treureN _ [] = []
treureN n (_:xs) = treureN (n - 1) xs

actualitzarPosi :: Int -> a -> [a] -> [a]
actualitzarPosi i elem xs = (primersN i xs)++[elem]++(treureN (i+1) xs) -- no funciona com deberia

first :: (a,b) -> a
first (a,b) = a
second :: (a,b) -> b
second (a,b) = b

elemsEnPosicioParell :: [a] -> [a]
elemsEnPosicioParell xs = [b | (a,b) <- zip [1..] xs, even a]
