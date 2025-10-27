-- 1.1 Expressions de tipus
data Bit = O | I
  deriving (Show)

(.==.) :: Bit -> Bit -> Bool
O .==. O = True
I .==. I = True
_ .==. _ = False

-- 1.2 Constructors de valors paramètrics
data DBit = D Bit Bit
  deriving (Show)

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
  deriving (Show)

natAint :: Nat -> Int -- Comptar els S fins que sigui Zero
natAint Zero = 0
natAint (S n) = 1 + natAint n

sumaNat :: Nat -> Nat -> Nat -- Extraure S del primer Nat fins que sigui Zero (i tindres el Nat suma dels dos a la trucada recursiva)
sumaNat Zero n = n
sumaNat m Zero = m
sumaNat (S m) n = S (sumaNat m n)

data MInt = Pos Nat | Neg Nat | MZero
