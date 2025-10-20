-- Exercici: definir les funcions (.) i ($)
(...) :: (b -> c) -> (a -> b) -> a -> c
(...) f g = \x -> f (g x)

(.$.) :: (a -> b) -> a -> b
(.$.) f x = f x

-- ghci> :t 2.43523 * 10 ^ 3
-- 2.43523 * 10 ^ 3 :: Fractional a => a
-- ghci> :t round (2.43523 * 10 ^ 3
-- round (2.43523 * 10 ^ 3) :: Integral b => b
-- ghci> :t fromIntegral . round $ 2.43523 * 10 ^ 3
-- fromIntegral . round $ 2.43523 * 10 ^ 3 :: Num c => c

-- Exercici: definir la funció arrodonir
arrodonir :: Double -> Integer -> Double
arrodonir valor numeroDecimals = (fromIntegral ... round .$. (valor * 10 ^ numeroDecimals)) / 10 ^ numeroDecimals

arrodonirLet :: Double -> Integer -> Double
arrodonirLet valor numeroDecimals =
  let
    zerosDecimals = 10 ^ numeroDecimals
    arrodonirADouble = (...) fromIntegral round
  in arrodonirADouble .$. (valor * zerosDecimals) / zerosDecimals

-- Exercici: definir la funció arrodonirb
-- Els parentesis necessaris son diferents perque $ associativa per la dreta i .$. per l'esquerra
arrodonirb :: Double -> Integer -> Double
arrodonirb valor numeroDecimals = (fromIntegral . round $ valor * 10 ^ numeroDecimals) / 10 ^ numeroDecimals

-- Exercici de composició
-- 1. Definir la funció elevar al quadrat
quad :: Int -> Int
quad x = x * x

-- 2. Definir la funció multiplicar
multiplicar :: Int -> Int -> Int
multiplicar x y = x * y

-- 3. Definir la funció triple
triple :: Int -> Int
triple x = x * 3

-- 4. Definir la funció triplequad (3n)^2
triplequad :: Int -> Int
triplequad = quad . triple

-- 5. Definir la funció multiplicarquad (n*m)^2
multiplicarquad :: Int -> Int -> Int
multiplicarquad m n = quad $ multiplicar m n -- recorda que $ es associativa per la dreta
