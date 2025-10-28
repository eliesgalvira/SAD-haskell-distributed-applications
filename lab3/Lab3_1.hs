module Lab3_1 where

-- 1.1 Expressions de tipus
data Bit = O | I
  deriving (Show, Eq)

(.==.) :: Bit -> Bit -> Bool
O .==. O = True
I .==. I = True
_ .==. _ = False

-- 1.2 Constructors de valors paramètrics
data DBit = D Bit Bit
  deriving (Show, Eq)

bitAint :: Bit -> Int
bitAint O = 0
bitAint I = 1

dbitAint :: DBit -> Int
dbitAint (D x y) = if x .==. O then bitAint y else 2 + bitAint y

-- 1.3 Patrons
dbitAintP :: DBit -> Int
dbitAintP (D O y) = bitAint y
dbitAintP (D I y) = 2 + bitAint y

-- 1.4 Definicions recursives
data Nat = Zero | S Nat -- Cada S afegeix 1 al Nat, el succeeix
  deriving (Show, Eq)

natAint :: Nat -> Int -- Comptar els S fins que sigui Zero
natAint Zero = 0
natAint (S n) = 1 + natAint n

sumaNat :: Nat -> Nat -> Nat -- Extraure S del primer Nat fins que sigui Zero (i tindres el Nat suma dels dos a la trucada recursiva)
sumaNat Zero n = n
sumaNat m Zero = m
sumaNat (S m) n = S (sumaNat m n)

data MInt = Pos Nat | Neg Nat -- N'hi ha dos zeros, el Pos Zero i el Neg Zero
  deriving (Show, Eq)

intAnat :: Int -> Nat -- No contemplo el cas negatiu, per fer
intAnat 0 = Zero
intAnat n = S (intAnat (n - 1))

mintAint :: MInt -> Int
mintAint (Pos n) = natAint n
mintAint (Neg n) = - natAint n

intAmint :: Int -> MInt
intAmint 0 = Pos Zero
intAmint n = if n > 0 then Pos (intAnat n) else Neg (intAnat (-n))

sumaMInt :: MInt -> MInt -> MInt
sumaMInt (Pos m) (Pos n) = intAmint (natAint m + natAint n)
sumaMInt (Neg m) (Neg n) = intAmint (-(natAint m + natAint n))
sumaMInt (Pos m) (Neg n) = intAmint (natAint m - natAint n)
sumaMInt (Neg m) (Pos n) = intAmint (natAint n - natAint m)

-- Auxiliars per Nat (sense Int: per comparar i resta pura)
eqNat :: Nat -> Nat -> Bool
eqNat Zero Zero = True
eqNat (S m) (S n) = eqNat m n
eqNat _ _ = False

ltNat :: Nat -> Nat -> Bool  -- m < n
ltNat Zero (S _) = True
ltNat (S m) Zero = False
ltNat (S m) (S n) = ltNat m n
ltNat Zero Zero = False

restaNat :: Nat -> Nat -> Nat  -- m - n, assumint m >= n (torna Zero si m < n, però nosaltres controlem)
restaNat Zero _ = Zero
restaNat m Zero = m
restaNat (S m) (S n) = restaNat m n

-- sumaMIntD: Sense conversions (només patrons, recursió i auxiliars)
-- Zero implícit: Pos Zero + qualsevol = qualsevol, etc.
sumaMIntD :: MInt -> MInt -> MInt
-- Casos amb zero (Pos Zero)
sumaMIntD (Pos Zero) n = n
sumaMIntD m (Pos Zero) = m

-- Pos + Pos
sumaMIntD (Pos m) (Pos n) = Pos (sumaNat m n)

-- Neg + Neg
sumaMIntD (Neg m) (Neg n) = Neg (sumaNat m n)

-- Pos + Neg: m - n (captura zero com Pos Zero)
sumaMIntD (Pos m) (Neg n) =
  if eqNat m n
    then Pos Zero  -- m == n → 0
    else if ltNat m n
           then Neg (restaNat n m)  -- m < n → -(n - m)
           else Pos (restaNat m n)  -- m > n → +(m - n)

-- Neg + Pos: n - m (simètric, captura zero com Pos Zero)
sumaMIntD (Neg m) (Pos n) =
  if eqNat m n
    then Pos Zero  -- m == n → 0
    else if ltNat m n
           then Pos (restaNat n m)  -- m < n → n - m > 0
           else Neg (restaNat m n)  -- m > n → -(m - n) < 0

-- Llista buida o conté un element i una llista (que pot ser buida)
data Llista a = B | L a (Llista a)
  deriving (Show, Eq)

-- Conversió entre la nostra Llista i les llistes d'Haskell
desdeL :: Llista a -> [a]
desdeL B = []
desdeL (L x xs) = x : desdeL xs

aL :: [a] -> Llista a
aL [] = B
aL (x:xs) = L x (aL xs)

-- Crea una llista amb els enters de n fins a 1
initL :: Int -> Llista Int
initL n
  | n <= 0 = B
  | otherwise = L n (initL (n - 1))

-- Gira una llista (tail-recursiu amb acumulador)
giraL :: Llista a -> Llista a
giraL xs = rev xs B
  where
    rev :: Llista a -> Llista a -> Llista a
    rev B acc = acc
    rev (L y ys) acc = rev ys (L y acc)

-- Llista de llistes: [[n..1],[n-1..1],..., [1]] amb el nostre tipus
initLdL :: Int -> Llista (Llista Int)
initLdL n
  | n <= 0 = B
  | otherwise = L (initL n) (initLdL (n - 1))

-- Conversió d'una Llista de Llistes a llista d'Haskell
desdeLdL :: Llista (Llista a) -> [[a]]
desdeLdL B = []
desdeLdL (L l ls) = desdeL l : desdeLdL ls

-- Concatena dues Llistes
concatenaL :: Llista a -> Llista a -> Llista a
concatenaL B ys = ys
concatenaL (L x xs) ys = L x (concatenaL xs ys)

-- Aplana una Llista de Llistes en una sola Llista
aplastaL :: Llista (Llista a) -> Llista a
aplastaL B = B
aplastaL (L l ls) = concatenaL l (aplastaL ls)

-- Mapeig sobre Llistes
mapejaL :: (a -> b) -> Llista a -> Llista b
mapejaL _ B = B
mapejaL f (L x xs) = L (f x) (mapejaL f xs)

-- Versions no recursives utilitzant mapejaL
initLdL2 :: Int -> Llista (Llista Int)
initLdL2 n = mapejaL initL (aL [n, n-1 .. 1])

desdeLdL2 :: Llista (Llista a) -> [[a]]
desdeLdL2 lls = desdeL (mapejaL desdeL lls)
