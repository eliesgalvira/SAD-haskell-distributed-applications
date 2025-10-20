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
arrodonirb :: Double -> Integer -> Double
arrodonirb valor numeroDecimals = (fromIntegral . round $ valor * 10 ^ numeroDecimals) / 10 ^ numeroDecimals
