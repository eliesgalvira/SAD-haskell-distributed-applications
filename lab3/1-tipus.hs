-- Expressions de tipus

data Bit = O | I
  deriving (Show)

(.==.) :: Bit -> Bit -> Bool
O .==. O = True
I .==. I = True
_ .==. _ = False
