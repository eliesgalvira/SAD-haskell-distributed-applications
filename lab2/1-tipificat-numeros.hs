-- Definicions inicials sense tipificar explícitament
p = 2 -- p :: Integer

q = 1 -- q :: Integer

dr = fromIntegral p / fromIntegral q -- dr :: Double

-- ghci> :t (/)
-- (/) :: Fractional a => a -> a -> a
-- ghci> :t div
-- div :: Integral a => a -> a -> a

d = div p q
