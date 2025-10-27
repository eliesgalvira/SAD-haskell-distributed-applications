-- 1.1 Expressions de tipus
data Bit = O | I
  deriving (Show)

(.==.) :: Bit -> Bit -> Bool
O .==. O = True
I .==. I = True
_ .==. _ = False

-- 1.2 Constructors de valors paramètrics
data Dbit = D Bit Bit
  deriving (Show)

bitAint :: Bit -> Int
bitAint O = 0
bitAint I = 1

dbitAint :: Dbit -> Int
dbitAint (D x y) = if x .==. O then bitAint y else 2 + bitAint y

-- 1.3 Patrons
dbitAintP :: Dbit -> Int
dbitAintP (D O y) = bitAint y
dbitAintP (D I y) = 2 + bitAint y
