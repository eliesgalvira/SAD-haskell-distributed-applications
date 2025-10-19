-- Definicions inicials sense tipificar explícitament
p = 2 -- p :: Integer, definit dr: p :: Double

q = 1 -- q :: Integer, definit dr: q :: Double

dr = fromIntegral p / fromIntegral q -- dr :: Double

-- ghci> :t (/)
-- (/) :: Fractional a => a -> a -> a
-- ghci> :t div
-- div :: Integral a => a -> a -> a


-- Si ara afegim: d = div p q
-- No compilarà, perquè p i q han quedat com a Double (per (/) i dr),
-- mentre que div requereix un tipus Integral.

d = div p q

dr2 = realToFrac p / realToFrac q
