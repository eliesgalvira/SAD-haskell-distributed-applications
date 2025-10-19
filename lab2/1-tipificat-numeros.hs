-- Definicions inicials sense tipificar explícitament
p = 2
-- Tipus inferit pel compilador:
-- p :: Num a => a

q = 1
-- Tipus inferit pel compilador:
-- q :: Num a => a

dr = p / q
-- Tipus inferit ara:
-- dr :: Fractional a => a
-- Explicació: (/) té tipus Fractional a => a -> a -> a
-- Per tant força que p i q siguin del tipus 'a' amb 'a' ∈ Fractional
-- (p. ex. Double, Float). Això especialitza la polisèmia de p i q.

-- Si afegim "d = div p q" sense més, NO compila. Per què?
-- 'div' té tipus:
-- div :: Integral a => a -> a -> a
-- Mentre que (/) té tipus:
-- (/) :: Fractional a => a -> a -> a
-- El compilador no pot fer coincidir alhora que p i q siguin Integral (per a div)
-- i al mateix temps Fractional (per a /) en el mateix mòdul si comparteixen el
-- mateix símbol polimòrfic. Necessitem ajudar-lo amb anotacions de tipus o
-- conversions.

-- Tipus de les funcions (escrivim-los com a comentari, obtinguts amb :t):
-- (/)  :: Fractional a => a -> a -> a
-- div  :: Integral a   => a -> a -> a

-- Solució 1: especialitzar p i q amb tipus concrets i separats

pI :: Int
pI = 2
qI :: Int
qI = 1

d :: Int
d = div pI qI
-- Ara compila perquè tot és Integral (Int)

pF :: Double
pF = 2
qF :: Double
qF = 1

dr2 :: Double
dr2 = pF / qF
-- Ara compila perquè tot és Fractional (Double)

-- Solució 2: mantenir p i q polimòrfics però convertir explícitament
-- Quan volem usar (/) amb enters, cal convertir-los a un tipus Fractional.
-- Es pot fer amb fromIntegral :: (Integral a, Num b) => a -> b

dr3 :: Double
dr3 = fromIntegral pI / fromIntegral qI

-- I quan volem usar 'div' però tenim valors en Double, primer cal anar
-- de Double a un enter. Per a un exemple senzill (i conscient que pot truncar):
d2 :: Int
d2 = div (floor pF) (floor qF)

-- Nota: la pàgina https://wiki.haskell.org/Converting_numbers llista funcions com
-- fromIntegral, realToFrac, ceiling, floor, round, etc., per a conversions entre
-- tipus numèrics.