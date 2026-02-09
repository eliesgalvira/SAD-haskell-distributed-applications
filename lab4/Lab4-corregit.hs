-- Nota: 8/10
module Lab4-corregit where

-- Importació necessària: utilitzem els tipus Llista i Nat definits al lab 3
import Lab3_1
import Data.Foldable
import Data.List (partition, sort)

-- ============================================================================
-- Secció 1: Classes i instàncies
-- ============================================================================

-- Classe Iterador per a tipus contenidors de valors
class Iterador t where
    ele :: t a -> a  -- Retorna el primer element
    seg :: t a -> t a  -- Elimina el primer element
    hasnext :: t a -> Bool  -- False si buit, True altrament

-- Instància de Iterador per Llista
instance Iterador Llista where
    ele (L x _) = x
    seg (L _ xs) = xs
    hasnext B = False
    hasnext (L _ _) = True

-- Funció sumElem usant l'iterador
sumElem :: (Iterador t, Num a) => t a -> a
sumElem contenedor
    | hasnext contenedor = ele contenedor + sumElem (seg contenedor)
    | otherwise = 0

-- Instàncies de Eq, Ord i Enum per Nat (Eq ja estava derivat)
instance Ord Nat where
    compare Zero Zero = EQ
    compare Zero (S _) = LT
    compare (S _) Zero = GT
    compare (S m) (S n) = compare m n

instance Enum Nat where
    toEnum 0 = Zero
    toEnum n
        | n > 0 = S (toEnum (n - 1))
        | otherwise = error "Nat: toEnum with negative number"
    
    fromEnum Zero = 0
    fromEnum (S n) = 1 + fromEnum n

-- ============================================================================
-- Secció 2: Les classes Semigroup i Monoid
-- ============================================================================

-- Instància de Semigroup per Nat
instance Semigroup Nat where
    (<>) = sumaNat

-- Instància de Monoid per Nat
instance Monoid Nat where
    mempty = Zero

-- ============================================================================
-- Secció 3: La classe Foldable
-- ============================================================================

-- Instància de Foldable per Llista
instance Foldable Llista where
    foldMap f B = mempty
    foldMap f (L x xs) = f x <> foldMap f xs
    
    foldr _ v B = v
    foldr f v (L x xs) = f x (foldr f v xs)
    
    foldl _ v B = v
    foldl f v (L x xs) = foldl f (f v x) xs

-- Funció foldNat per a aplicar una funció recursivament sobre Nat
foldNat :: (a -> a) -> a -> Nat -> a
foldNat _ z Zero = z
foldNat f z (S n) = f (foldNat f z n)

-- Redefinir natAint i sumNat usant foldNat
natAint' :: Nat -> Int
natAint' = foldNat (+1) 0

sumNat' :: Nat -> Nat -> Nat
sumNat' m n = foldNat S n m

-- ============================================================================
-- Secció 3.1: Exercicis amb llistes
-- ============================================================================

-- Calcular el màxim d'una llista d'enters
maxInt :: [Int] -> Int
maxInt [] = error "maxInt: llista buida"
maxInt xs = maximum xs

-- Comptar el número de Nothings en una llista de Maybe
countNothings :: [Maybe a] -> Int
countNothings = length . filter (maybe True (const False)) -- i amb fold?

-- Retornar una tupla amb l'element màxim i mínim
maxMin :: [Int] -> (Int, Int)
maxMin [] = error "maxMin: llista buida"
maxMin xs = (maximum xs, minimum xs)

-- Retornar una tupla amb el número d'elements i la seva suma
countSum :: [Int] -> (Int, Int)
countSum xs = (length xs, sum xs)

-- Mitjana aritmètica
mitjana :: [Int] -> Double
mitjana [] = error "mitjana: llista buida"
mitjana xs = fromIntegral (sum xs) / fromIntegral (length xs)

-- Totes les aparicions del valor màxim
totsMax :: [Int] -> [Int]
totsMax [] = []
totsMax xs = 
    let maxVal = maximum xs -- i sense calcular el màxim?
    in filter (== maxVal) xs

-- Posicions on apareix un caràcter en un String
-- Les posicions comencen des de 0 però l'exemple mostra [1,3,5] per "patata"
-- que correspon a les posicions 0-indexed: 1, 3, 5
posicionsLletra :: Char -> String -> [Int]
posicionsLletra c str = 
    let indexed = zip [0..] str -- i sense zip?
    in map fst (filter ((== c) . snd) indexed)

-- Suma de cada subllista
sumPar :: [[Int]] -> [Int]
sumPar = map sum

-- Suma total de tots els elements
sumTot :: [[Int]] -> Int
sumTot = sum . concat

-- Separar una llista segons un valor
-- Retorna una tupla amb els valors menors o iguals i la resta
separar :: Ord a => [a] -> a -> ([a], [a])
separar xs pivot = 
    let (menors, majors) = partition (<= pivot) xs
    in (sort menors, sort majors) -- cal ordenar?

-- Ordenar una llista (quicksort)
ordenar :: Ord a => [a] -> [a]
ordenar [] = []
ordenar (x:xs) = 
    let (menors, majors) = partition (< x) xs -- pensat per fer servir separar...
    in ordenar menors ++ [x] ++ ordenar majors

