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

-- 1.6 Llistes
data List a = Nil | Cons a (List a)
  deriving (Show, Eq)

-- Conversions between custom List and Haskell lists
desdeL :: List a -> [a]
desdeL Nil = []
desdeL (Cons x xs) = x : desdeL xs

aL :: [a] -> List a
aL [] = Nil
aL (x:xs) = Cons x (aL xs)

-- Create a custom list with integers from n down to 1
initL :: Int -> List Int
initL n
  | n <= 0 = Nil
  | otherwise = Cons n (initL (n - 1))

-- Reverse a custom list
giraL :: List a -> List a
giraL xs = go xs Nil
  where
    go :: List a -> List a -> List a
    go Nil acc = acc
    go (Cons y ys) acc = go ys (Cons y acc)

-- List of lists: [[n..1], [n-1..1], ..., [1]] in custom List
initLdL :: Int -> List (List Int)
initLdL n
  | n <= 0 = Nil
  | otherwise = Cons (initL n) (initLdL (n - 1))

-- Convert a custom list of custom lists to a Haskell list of lists
desdeLdL :: List (List a) -> [[a]]
desdeLdL Nil = []
desdeLdL (Cons l ls) = desdeL l : desdeLdL ls

-- Append two custom lists
appendL :: List a -> List a -> List a
appendL Nil ys = ys
appendL (Cons x xs) ys = Cons x (appendL xs ys)

-- Flatten a custom list of custom lists into a single custom list
aplastaL :: List (List a) -> List a
aplastaL Nil = Nil
aplastaL (Cons l ls) = appendL l (aplastaL ls)

-- Map over custom lists
mapejaL :: (a -> b) -> List a -> List b
mapejaL _ Nil = Nil
mapejaL f (Cons x xs) = Cons (f x) (mapejaL f xs)

-- Non-recursive versions using mapejaL
initLdL2 :: Int -> List (List Int)
initLdL2 n = mapejaL initL (aL [n, n-1 .. 1])

desdeLdL2 :: List (List a) -> [[a]]
desdeLdL2 lls = desdeL (mapejaL desdeL lls)
