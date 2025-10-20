-- Exercici: definir les funcions (.) i ($)
(.$.) :: (b -> c) -> (a -> b) -> a -> c
(.$.) f g = \x -> f (g x)

(...) :: (a -> b) -> a -> b
(...) f = f

-- Exercici: definir la funció arrodonir
arrodonir :: Double -> Integer -> Double
arrodonir valor numeroDecimals = (fromIntegral . round $ valor * 10 ^ numeroDecimals) / 10 ^ numeroDecimals
